use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take, take_until},
    character::{
        complete::{newline, space0},
        streaming::multispace0,
    },
    combinator::{eof, opt, recognize},
    error::{context, VerboseError},
    multi::{count, many0, many_till, separated_list1},
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
    pub dependencies: Vec<Entry<'a>>,
    pub descriptors: Vec<&'a str>,
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

fn parse_entry(input: &str) -> Res<&str, Entry> {
    context("entry", tuple((entry_descriptors, entry_version)))(input).map(|(next_input, res)| {
        let (descriptors, version) = res;

        // descriptors is guaranteed to be of length >= 1
        let first_descriptor = descriptors.get(0).expect("Somehow descriptors is empty");

        // XXX TODO should not use `expect`, but we are mapping the `ok` part
        let (_, name) = entry_name(first_descriptor).expect("Error parsing name");

        (
            next_input,
            Entry {
                name,
                version,
                descriptors,
                ..Default::default()
            },
        )
    })
}

fn entry_name(input: &str) -> Res<&str, &str> {
    let (i, _) = opt(tag(r#"""#))(input)?;
    let opt_at = opt(tag("@"));
    let name = tuple((opt_at, take_until("@")));
    context("name", recognize(name))(i)
}

fn entry_single_descriptor(input: &str) -> Res<&str, &str> {
    context(
        "single_descriptor",
        alt((
            delimited(tag("\""), take_until("\""), tag("\"")),
            is_not(",:"),
        )),
    )(input)
}

fn entry_descriptors(input: &str) -> Res<&str, Vec<&str>> {
    context(
        "descriptors",
        terminated(
            separated_list1(tag(", "), entry_single_descriptor),
            tag(":"),
        ),
    )(input)
}

fn entry_version(input: &str) -> Res<&str, &str> {
    let (i, _) = take_until(r#"version ""#)(input)?;
    context(
        "version",
        delimited(tag(r#"version ""#), is_version, tag(r#"""#)),
    )(i)
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
                descriptors: vec!["@babel/code-frame@^7.0.0"],
                ..Default::default()
            }
        );

        assert_eq!(
            res.1.last().unwrap(),
            &Entry {
                name: "yargs",
                version: "9.0.1",
                descriptors: vec!["yargs@^9.0.0"],
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
                    descriptors: vec!["@babel/code-frame@^7.0.0"],
                    ..Default::default()
                },
                Entry {
                    name: "@babel/helper-validator-identifier",
                    version: "7.12.11",
                    descriptors: vec!["@babel/helper-validator-identifier@^7.12.11"],
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
                descriptors: vec!["@babel/code-frame@^7.0.0"],
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
                descriptors: vec!["@babel/helper-validator-identifier@^7.12.11"],
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
                descriptors: vec!["@babel/helper-validator-identifier@^7.12.11"],
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
                descriptors: vec!["@babel/code-frame@^7.0.0"],
                ..Default::default()
            },
        );
    }

    #[test]
    fn entry_version_works() {
        fn assert(input: &str, expect: &str) {
            let res = entry_version(input).unwrap();
            assert_eq!(res.1, expect);
        }

        assert(
            r#"@^7.0.0":
    version "7.12.13"
    resolved "https://registry.yarnpkg.com/@babel/code-frame/-/code-frame-7.12.13.tgz#dcfc826beef65e75c50e21d3837d7d95798dd658"
    integrity sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g==
    dependencies:
        "@babel/highlight" "^7.12.13""#,
            "7.12.13",
        );
    }

    #[test]
    fn entry_name_works() {
        fn assert(input: &str, expect: &str) {
            let res = entry_name(input).unwrap();
            assert_eq!(res.1, expect);
        }

        assert(
            r#""@babel/code-frame@^7.0.0":
    version "7.12.13"
    resolved "https://registry.yarnpkg.com/@babel/code-frame/-/code-frame-7.12.13.tgz#dcfc826beef65e75c50e21d3837d7d95798dd658"
    integrity sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g==
    dependencies:
        "@babel/highlight" "^7.12.13""#,
            "@babel/code-frame",
        );
        assert(r#""@babel/code-frame@^7.0.0":"#, "@babel/code-frame");
        assert(
            r#""@babel/helper-validator-identifier@^7.12.11":"#,
            "@babel/helper-validator-identifier",
        );
        assert(r#"ansi-escapes@^3.0.0:"#, "ansi-escapes");
        assert(r#"arr-flatten@^1.0.1, arr-flatten@^1.1.0:"#, "arr-flatten");
    }

    #[test]
    fn entry_descriptors_works() {
        fn assert(input: &str, expect: Vec<&str>) {
            let res = entry_descriptors(input).unwrap();
            assert_eq!(res.1, expect);
        }

        assert(
            r#"abab@^1.0.3:
            version "1.0.4"
        "#,
            vec!["abab@^1.0.3"],
        );

        assert(
            r#""@nodelib/fs.stat@2.0.3":
            version "2.0.3"
        "#,
            vec!["@nodelib/fs.stat@2.0.3"],
        );

        assert(
            r#"abab@^1.0.3, abab@^1.0.4:
            version "1.0.4"
        "#,
            vec!["abab@^1.0.3", "abab@^1.0.4"],
        );

        assert(
            r#""@nodelib/fs.stat@2.0.3", "@nodelib/fs.stat@^2.0.2":
            version "2.0.3"
        "#,
            vec!["@nodelib/fs.stat@2.0.3", "@nodelib/fs.stat@^2.0.2"],
        );
    }
}
