use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take, take_until},
    character::{
        complete::{line_ending, newline, space0, space1},
        streaming::multispace0,
    },
    combinator::{eof, recognize},
    error::{context, VerboseError},
    multi::{count, many0, many1, many_till, separated_list1},
    sequence::{delimited, terminated, tuple},
    AsChar, IResult,
};

use thiserror::Error;

type Res<T, U> = IResult<T, U, VerboseError<T>>;

/// Parser error
#[derive(Debug, Error)]
#[error("yarn.lock error")]
pub enum YarnLockError {
    #[error("Error parsing yarn.lock file")]
    Parser {
        #[from]
        source: nom::Err<VerboseError<String>>,
    },
}

/// yarn.lock entry.
/// It only shows the name of the dependency and the version.
#[derive(Debug, PartialEq, Eq, Default)]
pub struct Entry<'a> {
    pub name: &'a str,
    pub version: &'a str,
    pub dependencies: Vec<(&'a str, &'a str)>,
    pub descriptors: Vec<(&'a str, &'a str)>,
}

/// Accepts the `yarn.lock` content and returns all the entries.
pub fn parse_str(content: &str) -> Result<Vec<Entry>, YarnLockError> {
    parse(content).map(|(_, entries)| entries).map_err(|e| {
        e.map(|ve| {
            let errors = ve
                .errors
                .into_iter()
                .map(|v| (v.0.to_string(), v.1))
                .collect();
            VerboseError { errors }
        })
        .into()
    })
}

fn parse(input: &str) -> Res<&str, Vec<Entry>> {
    let (i, _) = yarn_lock_header(input)?;
    let (i, _) = opt(yarn_lock_metadata)(i)?;
    let (i, mut entries) = many0(entry)(i)?;
    let (i, final_entry) = entry_final(i)?;
    entries.push(final_entry);
    Ok((i, entries))
}

fn take_till_line_end(input: &str) -> Res<&str, &str> {
    recognize(tuple((
        alt((take_until("\n"), take_until("\n\r"))),
        take(1usize),
    )))(input)
}

fn yarn_lock_header(input: &str) -> Res<&str, &str> {
    recognize(tuple((count(take_till_line_end, 2), multispace0)))(input)
}

fn yarn_lock_metadata(input: &str) -> Res<&str, &str> {
    context(
        "metadata",
        recognize(tuple((
            tag("__metadata:"),
            take_till_line_end,
            many_till(take_till_line_end, recognize(tuple((space0, newline)))),
            multispace0,
        ))),
    )(input)
}

fn entry_final(input: &str) -> Res<&str, Entry> {
    recognize(many_till(take_till_line_end, eof))(input).map(|(i, capture)| {
        let (_, my_entry) = parse_entry(capture).expect("Error parsing Entry");
        (i, my_entry)
    })
}

fn entry(input: &str) -> Res<&str, Entry> {
    recognize(many_till(
        take_till_line_end,
        recognize(tuple((space0, newline))),
    ))(input)
    .map(|(i, capture)| {
        let (_, my_entry) = parse_entry(capture).expect("Error parsing Entry");
        (i, my_entry)
    })
}

#[derive(PartialEq, Debug)]
enum EntryItem<'a> {
    Version(&'a str),
    Dependencies(Vec<(&'a str, &'a str)>),
    Unknown(&'a str),
}

fn unknown_line(input: &str) -> Res<&str, EntryItem> {
    take_till_line_end(input).map(|(i, res)| (i, EntryItem::Unknown(res)))
}

fn entry_item(input: &str) -> Res<&str, EntryItem> {
    alt((entry_version, parse_dependencies, unknown_line))(input)
}

fn parse_entry(input: &str) -> Res<&str, Entry> {
    context("entry", tuple((entry_descriptors, many1(entry_item))))(input).map(
        |(next_input, res)| {
            let (descriptors, entry_items) = res;

            // descriptors is guaranteed to be of length >= 1
            let first_descriptor = descriptors.get(0).expect("Somehow descriptors is empty");

            let name = first_descriptor.0;

            let mut version = "";
            let mut dependencies = Vec::new();

            for ei in entry_items {
                match ei {
                    EntryItem::Version(v) => version = v,
                    EntryItem::Dependencies(d) => dependencies = d,
                    EntryItem::Unknown(_) => (),
                }
            }

            assert_ne!(version, "");

            (
                next_input,
                Entry {
                    name,
                    version,
                    dependencies,
                    descriptors,
                },
            )
        },
    )
}

fn parse_dependencies(input: &str) -> Res<&str, EntryItem> {
    let (input, (indent, _, _)) = tuple((space1, tag("dependencies:"), line_ending))(input)?;

    let dependencies_parser = many1(move |i| {
        tuple((
            tag(indent),                    // indented as much as the parent...
            space1,                         // ... plus extra indentation
            double_quoted_text_or_unquoted, // package name
            space1,
            double_quoted_text, // version
            line_ending,
        ))(i)
        .map(|(i, (_, _, p, _, v, _))| (i, (p, v)))
    });
    context("dependencies", dependencies_parser)(input)
        .map(|(i, res)| (i, EntryItem::Dependencies(res)))
}

/**
 * Simple version, it doesn't consider escaped quotes since in our scenarios
 * it can't happen.
 */
fn double_quoted_text(input: &str) -> Res<&str, &str> {
    delimited(tag("\""), take_until("\""), tag("\""))(input)
}

fn space_delimited_word(input: &str) -> Res<&str, &str> {
    is_not(" \t\r\n")(input)
}

fn double_quoted_text_or_unquoted(input: &str) -> Res<&str, &str> {
    alt((double_quoted_text, space_delimited_word))(input)
}

fn entry_single_descriptor(input: &str) -> Res<&str, &str> {
    context("single_descriptor", alt((double_quoted_text, is_not(",:"))))(input)
}

fn entry_descriptors(input: &str) -> Res<&str, Vec<(&str, &str)>> {
    context(
        "descriptors",
        terminated(
            separated_list1(tag(", "), entry_single_descriptor),
            tuple((tag(":"), line_ending)),
        ),
    )(input)
    .map(|(i, res)| {
        let x = res
            .into_iter()
            .map(|desc: &str| desc.rsplit_once('@').unwrap())
            .collect();
        (i, x)
    })
}

fn entry_version(input: &str) -> Res<&str, EntryItem> {
    context(
        "version",
        tuple((
            space1,
            tag("version"),
            opt(tag(":")),
            space1,
            opt(tag("\"")),
            is_version,
            opt(tag("\"")),
            line_ending,
        )),
    )(input)
    .map(|(i, (_, _, _, _, _, version, _, _))| (i, EntryItem::Version(version)))
}

fn is_version<T, E: nom::error::ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: nom::InputTakeAtPosition,
    <T as nom::InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position1_complete(
        |item| {
            let c: char = item.as_char();
            !(c == '.' || c == '-' || c.is_alphanum())
        },
        nom::error::ErrorKind::AlphaNumeric,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_doc_from_file_works() {
        let content = std::fs::read_to_string("yarn.lock").unwrap();
        let res = parse(&content).unwrap();

        assert_eq!(res.0, "");
        assert_eq!(
            res.1.first().unwrap(),
            &Entry {
                name: "@babel/code-frame",
                version: "7.12.13",
                descriptors: vec![("@babel/code-frame", "^7.0.0")],
                dependencies: vec![("@babel/highlight", "^7.12.13")],
                ..Default::default()
            }
        );

        assert_eq!(
            res.1.last().unwrap(),
            &Entry {
                name: "yargs",
                version: "9.0.1",
                descriptors: vec![("yargs", "^9.0.0")],
                dependencies: vec![
                    ("camelcase", "^4.1.0"),
                    ("cliui", "^3.2.0"),
                    ("decamelize", "^1.1.1"),
                    ("get-caller-file", "^1.0.1"),
                    ("os-locale", "^2.0.0"),
                    ("read-pkg-up", "^2.0.0"),
                    ("require-directory", "^2.1.1"),
                    ("require-main-filename", "^1.0.1"),
                    ("set-blocking", "^2.0.0"),
                    ("string-width", "^2.0.0"),
                    ("which-module", "^2.0.0"),
                    ("y18n", "^3.2.1"),
                    ("yargs-parser", "^7.0.0"),
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn parse_doc_from_memory_works() {
        fn assert(input: &str, expect: &[Entry]) {
            let res = parse(input).unwrap();
            assert_eq!(res.1, expect);
        }

        assert(
            r#"# THIS IS AN AUTOGENERATED FILE. DO NOT EDIT THIS FILE DIRECTLY.
# yarn lockfile v1


"@babel/code-frame@^7.0.0":
    version "7.12.13"
    resolved "https://registry.yarnpkg.com/@babel/code-frame/-/code-frame-7.12.13.tgz#dcfc826beef65e75c50e21d3837d7d95798dd658"
    integrity sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g==
        dependencies:
            "@babel/highlight" "^7.12.13"

"@babel/helper-validator-identifier@^7.12.11":
    version "7.12.11"
    resolved "https://registry.yarnpkg.com/@babel/helper-validator-identifier/-/helper-validator-identifier-7.12.11.tgz#c9a1f021917dcb5ccf0d4e453e399022981fc9ed"
    integrity sha512-np/lG3uARFybkoHokJUmf1QfEvRVCPbmQeUQpKow5cQ3xWrV9i3rUHodKDJPQfTVX61qKi+UdYk8kik84n7XOw==
"#,
            &[
                Entry {
                    name: "@babel/code-frame",
                    version: "7.12.13",
                    descriptors: vec![("@babel/code-frame", "^7.0.0")],
                    dependencies: vec![("@babel/highlight", "^7.12.13")],
                    ..Default::default()
                },
                Entry {
                    name: "@babel/helper-validator-identifier",
                    version: "7.12.11",
                    descriptors: vec![("@babel/helper-validator-identifier", "^7.12.11")],
                    ..Default::default()
                },
            ],
        );
    }

    #[test]
    fn entry_works() {
        fn assert(input: &str, expect: Entry) {
            let res = entry(input).unwrap();
            assert_eq!(res.1, expect);
        }

        assert(
            r#""@babel/code-frame@^7.0.0":
    version "7.12.13"
    resolved "https://registry.yarnpkg.com/@babel/code-frame/-/code-frame-7.12.13.tgz#dcfc826beef65e75c50e21d3837d7d95798dd658"
    integrity sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g==
    dependencies:
        "@babel/highlight" "^7.12.13"

 "#,
            Entry {
                name: "@babel/code-frame",
                version: "7.12.13",
                descriptors: vec![("@babel/code-frame", "^7.0.0")],
                dependencies: vec![("@babel/highlight", "^7.12.13")],
                ..Default::default()
            },
        );
        // with final spaces
        assert(
            r#""@babel/helper-validator-identifier@^7.12.11":
    version "7.12.11"
    resolved "https://registry.yarnpkg.com/@babel/helper-validator-identifier/-/helper-validator-identifier-7.12.11.tgz#c9a1f021917dcb5ccf0d4e453e399022981fc9ed"
    integrity sha512-np/lG3uARFybkoHokJUmf1QfEvRVCPbmQeUQpKow5cQ3xWrV9i3rUHodKDJPQfTVX61qKi+UdYk8kik84n7XOw==

 "#,
            Entry {
                name: "@babel/helper-validator-identifier",
                version: "7.12.11",
                descriptors: vec![("@babel/helper-validator-identifier", "^7.12.11")],
                ..Default::default()
            },
        );
        // without final spaces
        assert(
            r#""@babel/helper-validator-identifier@^7.12.11":
    version "7.12.11"
    resolved "https://registry.yarnpkg.com/@babel/helper-validator-identifier/-/helper-validator-identifier-7.12.11.tgz#c9a1f021917dcb5ccf0d4e453e399022981fc9ed"
    integrity sha512-np/lG3uARFybkoHokJUmf1QfEvRVCPbmQeUQpKow5cQ3xWrV9i3rUHodKDJPQfTVX61qKi+UdYk8kik84n7XOw==

"#,
            Entry {
                name: "@babel/helper-validator-identifier",
                version: "7.12.11",
                descriptors: vec![("@babel/helper-validator-identifier", "^7.12.11")],
                ..Default::default()
            },
        );
    }

    #[test]
    fn parse_entry_works() {
        fn assert(input: &str, expect: Entry) {
            let res = parse_entry(input).unwrap();
            assert_eq!(res.1, expect);
        }

        assert(
            r#""@babel/code-frame@^7.0.0":
    version "7.12.13"
    resolved "https://registry.yarnpkg.com/@babel/code-frame/-/code-frame-7.12.13.tgz#dcfc826beef65e75c50e21d3837d7d95798dd658"
    integrity sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g==
    dependencies:
        "@babel/highlight" "^7.12.13"

"#,
            Entry {
                name: "@babel/code-frame",
                version: "7.12.13",
                descriptors: vec![("@babel/code-frame", "^7.0.0")],
                dependencies: vec![("@babel/highlight", "^7.12.13")],
                ..Default::default()
            },
        );
    }

    #[test]
    fn entry_version_works() {
        assert_eq!(
            entry_version("  version \"1.2.3\"\n"),
            Ok(("", EntryItem::Version("1.2.3")))
        );
        assert_eq!(
            entry_version("  version \"1.2.3-beta1\"\n"),
            Ok(("", EntryItem::Version("1.2.3-beta1")))
        );
        assert_eq!(
            entry_version("  version: 1.2.3\n"),
            Ok(("", EntryItem::Version("1.2.3")))
        );
        assert!(entry_version("    node-version: 1.0.0\n").is_err());
    }

    #[test]
    fn entry_descriptors_works() {
        fn assert(input: &str, expect: Vec<(&str, &str)>) {
            let res = entry_descriptors(input).unwrap();
            assert_eq!(res.1, expect);
        }

        assert(
            r#"abab@^1.0.3:
            version "1.0.4"
        "#,
            vec![("abab", "^1.0.3")],
        );

        assert(
            r#""@nodelib/fs.stat@2.0.3":
            version "2.0.3"
        "#,
            vec![("@nodelib/fs.stat", "2.0.3")],
        );

        assert(
            r#"abab@^1.0.3, abab@^1.0.4:
            version "1.0.4"
        "#,
            vec![("abab", "^1.0.3"), ("abab", "^1.0.4")],
        );

        assert(
            r#""@nodelib/fs.stat@2.0.3", "@nodelib/fs.stat@^2.0.2":
            version "2.0.3"
        "#,
            vec![
                ("@nodelib/fs.stat", "2.0.3"),
                ("@nodelib/fs.stat", "^2.0.2"),
            ],
        );

        // yarn >= 2.0 format
        assert(
            r#""@nodelib/fs.stat@npm:2.0.3, @nodelib/fs.stat@npm:^2.0.2":
            version "2.0.3"
        "#,
            vec![
                ("@nodelib/fs.stat", "2.0.3"),
                ("@nodelib/fs.stat", "^2.0.2"),
            ],
        );
    }

    #[test]
    fn space_delimited_word_works() {
        let res = space_delimited_word("foo ");
        assert_eq!(res, Ok((" ", "foo")));
    }

    #[test]
    fn unknown_line_works() {
        let res = unknown_line("foo\nbar").unwrap();
        assert_eq!(res, ("bar", EntryItem::Unknown("foo\n")));
    }

    #[test]
    fn parse_dependencies_work() {
        fn assert(input: &str, expect: EntryItem) {
            let res = parse_dependencies(input).unwrap();
            assert_eq!(res.1, expect);
        }

        assert(
            r#"            dependencies:
                foo "1.0"
                "bar" "0.3-alpha1"
        "#,
            EntryItem::Dependencies(vec![("foo", "1.0"), ("bar", "0.3-alpha1")]),
        );
    }
}
