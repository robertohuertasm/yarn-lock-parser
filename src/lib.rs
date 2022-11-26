use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take, take_till, take_until},
    character::{
        complete::{line_ending, not_line_ending, one_of, space0, space1},
        streaming::multispace0,
    },
    combinator::{eof, fail, opt, recognize},
    error::{context, ParseError, VerboseError},
    multi::{count, many0, many1, many_till, separated_list1},
    sequence::{delimited, tuple},
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
    pub integrity: &'a str,
    pub dependencies: Vec<(&'a str, &'a str)>,
    pub descriptors: Vec<(&'a str, &'a str)>,
}

/// Accepts the `yarn.lock` content and returns all the entries.
/// # Errors
/// - `YarnLockError`
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
        alt((take_until("\n"), take_until("\r\n"))),
        take(1usize),
    )))(input)
}

fn take_till_optional_line_end(input: &str) -> Res<&str, &str> {
    recognize(tuple((
        alt((take_until("\n"), take_until("\r\n"), space0)),
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
            many_till(take_till_line_end, recognize(tuple((space0, line_ending)))),
            multispace0,
        ))),
    )(input)
}

fn entry_final(input: &str) -> Res<&str, Entry> {
    recognize(many_till(take_till_optional_line_end, eof))(input).map(|(i, capture)| {
        let (_, my_entry) = parse_entry(capture).expect("Error parsing Entry");
        (i, my_entry)
    })
}

fn entry(input: &str) -> Res<&str, Entry> {
    recognize(many_till(
        take_till_line_end,
        recognize(tuple((space0, line_ending))),
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
    Integrity(&'a str),
    Unknown(&'a str),
}

fn unknown_line(input: &str) -> Res<&str, EntryItem> {
    take_till_line_end(input).map(|(i, res)| (i, EntryItem::Unknown(res)))
}

fn integrity(input: &str) -> Res<&str, EntryItem> {
    context(
        "integrity",
        tuple((
            space1,
            opt(tag("\"")),
            alt((tag("checksum"), tag("integrity"))),
            opt(tag("\"")),
            opt(tag(":")),
            space1,
            opt(tag("\"")),
            take_till(|c| c == '"' || c == '\n' || c == '\r'),
        )),
    )(input)
    .map(|(i, (_, _, _, _, _, _, _, integrity))| (i, EntryItem::Integrity(integrity)))
}

fn entry_item(input: &str) -> Res<&str, EntryItem> {
    alt((entry_version, parse_dependencies, integrity, unknown_line))(input)
}

fn parse_entry(input: &str) -> Res<&str, Entry> {
    context("entry", tuple((entry_descriptors, many1(entry_item))))(input).map(
        |(next_input, res)| {
            let (descriptors, entry_items) = res;

            // descriptors is guaranteed to be of length >= 1
            let first_descriptor = descriptors.first().expect("Somehow descriptors is empty");

            let name = first_descriptor.0;

            let mut version = "";
            let mut dependencies = Vec::new();
            let mut integrity = "";

            for ei in entry_items {
                match ei {
                    EntryItem::Version(v) => version = v,
                    EntryItem::Dependencies(d) => dependencies = d,
                    EntryItem::Integrity(c) => integrity = c,
                    EntryItem::Unknown(_) => (),
                }
            }

            assert_ne!(version, "", "Version is empty");

            (
                next_input,
                Entry {
                    name,
                    version,
                    integrity,
                    dependencies,
                    descriptors,
                },
            )
        },
    )
}

fn dependency_version(input: &str) -> Res<&str, &str> {
    alt((double_quoted_text, not_line_ending))(input).map(|(i, version)| {
        (
            i,
            // e.g. 1.2.3
            // e.g. "1.2.3"
            // e.g. "npm:foo@1.0.0 || 1.0.1" # it happens with aliased deps
            version.rsplit_once('@').unwrap_or(("", version)).1,
        )
    })
}

fn parse_dependencies(input: &str) -> Res<&str, EntryItem> {
    let (input, (indent, _, _)) = tuple((space1, tag("dependencies:"), line_ending))(input)?;

    let dependencies_parser = many1(move |i| {
        tuple((
            tag(indent),  // indented as much as the parent...
            space1,       // ... plus extra indentation
            is_not(": "), // package name
            one_of(": "),
            space0,
            dependency_version,         // version
            alt((line_ending, space0)), // newline or space
        ))(i)
        .map(|(i, (_, _, p, _, _, v, _))| (i, (p.trim_matches('"'), v)))
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

fn entry_single_descriptor<'a>(input: &'a str) -> Res<&str, (&str, &str)> {
    let (i, (_, desc)) = tuple((opt(tag("\"")), is_not(",\"\n")))(input)?;
    let i = i.strip_prefix('"').unwrap_or(i);

    let (_, (name, version)) = context("entry single descriptor", |i: &'a str| {
        let version_start_idx = i.rfind('@').map(|idx| {
            let idx = idx + 1;
            // does it also contains a colon? e.g. foo@workspace:.
            i[idx..].rfind(':').map_or(idx, |new_idx| idx + new_idx + 1)
        });

        #[allow(clippy::manual_strip)]
        let name_end_idx = if i.starts_with('@') {
            i[1..].find('@').map(|idx| idx + 1)
        } else {
            i.find('@')
        };

        if name_end_idx.is_none() || version_start_idx.is_none() {
            return Err(nom::Err::Failure(
                nom::error::VerboseError::from_error_kind(
                    "version format error: @ not found",
                    nom::error::ErrorKind::Fail,
                ),
            ));
        };

        let (name, version) = (
            &i[..name_end_idx.unwrap()],
            &i[version_start_idx.unwrap()..],
        );

        Ok((i, (name, version)))
    })(desc)?;

    Ok((i, (name, version)))
}

fn entry_descriptors<'a>(input: &'a str) -> Res<&str, Vec<(&str, &str)>> {
    // foo@1:
    // "foo@npm:1.2":
    // "foo@1.2", "foo@npm:3.4":
    // "foo@npm:1.2, foo@npm:3.4":
    // "foo@npm:0.3.x, foo@npm:>= 0.3.2 < 0.4.0":

    context(
        "descriptors",
        |input: &'a str| -> Res<&str, Vec<(&str, &str)>> {
            let (input, line) = take_till_line_end(input)?;

            let line = line
                .strip_suffix(":\r\n")
                .or_else(|| line.strip_suffix(":\n"));

            if line.is_none() {
                fail::<_, &str, _>("descriptor does not end with : followed by newline")?;
            }
            let line = line.unwrap();

            let (_, res) =
                separated_list1(tuple((opt(tag("\"")), tag(", "))), entry_single_descriptor)(line)?;

            Ok((input, res))
        },
    )(input)
}

fn entry_version(input: &str) -> Res<&str, EntryItem> {
    // "version \"7.12.13\"\r\n"
    context(
        "version",
        tuple((
            space1,
            opt(tag("\"")),
            tag("version"),
            opt(tag("\"")),
            opt(tag(":")),
            space1,
            opt(tag("\"")),
            is_version,
            opt(tag("\"")),
            line_ending,
        )),
    )(input)
    .map(|(i, (_, _, _, _, _, _, _, version, _, _))| (i, EntryItem::Version(version)))
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

    fn assert_v1(res: (&str, Vec<Entry>)) {
        assert_eq!(res.0, "");
        assert_eq!(
            res.1.first().unwrap(),
            &Entry {
                name: "@babel/code-frame",
                version: "7.12.13",
                descriptors: vec![("@babel/code-frame", "^7.0.0")],
                dependencies: vec![("@babel/highlight", "^7.12.13")],
                integrity: "sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g=="
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
                integrity: "sha1-UqzCP+7Kw0BCB47njAwAf1CF20w="
            }
        );
    }

    #[test]
    fn parse_windows_server_from_memory_works() {
        let content = "# THIS IS AN AUTOGENERATED FILE. DO NOT EDIT THIS FILE DIRECTLY.\r\n# yarn lockfile v1\r\n\r\n\r\n\"@babel/code-frame@^7.0.0\":\r\n  version \"7.12.13\"\r\n  resolved \"https://registry.yarnpkg.com/@babel/code-frame/-/code-frame-7.12.13.tgz#dcfc826beef65e75c50e21d3837d7d95798dd658\"\r\n  integrity sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g==\r\n  dependencies:\r\n    \"@babel/highlight\" \"^7.12.13\"\r\n\r\nyargs-parser@^7.0.0:\r\n  version \"7.0.0\"\r\n  resolved \"https://registry.yarnpkg.com/yargs-parser/-/yargs-parser-7.0.0.tgz#8d0ac42f16ea55debd332caf4c4038b3e3f5dfd9\"\r\n  integrity sha1-jQrELxbqVd69MyyvTEA4s+P139k=\r\n  dependencies:\r\n    camelcase \"^4.1.0\"\r\n\r\nyargs@^9.0.0:\r\n  version \"9.0.1\"\r\n  resolved \"https://registry.yarnpkg.com/yargs/-/yargs-9.0.1.tgz#52acc23feecac34042078ee78c0c007f5085db4c\"\r\n  integrity sha1-UqzCP+7Kw0BCB47njAwAf1CF20w=\r\n  dependencies:\r\n    camelcase \"^4.1.0\"\r\n    cliui \"^3.2.0\"\r\n    decamelize \"^1.1.1\"\r\n    get-caller-file \"^1.0.1\"\r\n    os-locale \"^2.0.0\"\r\n    read-pkg-up \"^2.0.0\"\r\n    require-directory \"^2.1.1\"\r\n    require-main-filename \"^1.0.1\"\r\n    set-blocking \"^2.0.0\"\r\n    string-width \"^2.0.0\"\r\n    which-module \"^2.0.0\"\r\n    y18n \"^3.2.1\"\r\n    yargs-parser \"^7.0.0\"\r\n";
        let res = parse(&content).unwrap();
        assert_v1(res);
    }

    #[test]
    fn parse_v1_doc_from_file_works() {
        let content = std::fs::read_to_string("tests/v1/yarn.lock").unwrap();
        let res = parse(&content).unwrap();
        assert_v1(res);
    }

    #[test]
    fn parse_v1_doc_from_file_without_endline_works() {
        let content = std::fs::read_to_string("tests/v1_without_endline/yarn.lock").unwrap();
        let res = parse(&content).unwrap();
        assert_v1(res)
    }

    #[test]
    fn parse_v1_doc_from_file_with_npm_bug_works() {
        // SEE: https://github.com/robertohuertasm/yarn-lock-parser/issues/3
        let content = std::fs::read_to_string("tests/v1_with_npm_bug/yarn.lock").unwrap();
        let res = parse(&content).unwrap();
        // using v6 as we generated the lock file with v6 information.
        // the npm bug convert it back to v1.
        assert_v6(res, true);
    }

    #[test]
    fn parse_v1_doc_from_memory_works_v1() {
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
                    integrity: "sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g=="
                },
                Entry {
                    name: "@babel/helper-validator-identifier",
                    version: "7.12.11",
                    descriptors: vec![("@babel/helper-validator-identifier", "^7.12.11")],
                    integrity: "sha512-np/lG3uARFybkoHokJUmf1QfEvRVCPbmQeUQpKow5cQ3xWrV9i3rUHodKDJPQfTVX61qKi+UdYk8kik84n7XOw==",
                    ..Default::default()
                },
            ],
        );
    }

    fn assert_v6(res: (&str, Vec<Entry>), with_bug: bool) {
        assert_eq!(res.0, "");
        assert_eq!(
            res.1.first().unwrap(),
            &Entry {
                name: "@babel/code-frame",
                version: "7.18.6",
                descriptors: vec![("@babel/code-frame", "^7.18.6")],
                dependencies: vec![("@babel/highlight", "^7.18.6")],
                integrity: if with_bug {
                    "sha512-TDCmlK5eOvH+eH7cdAFlNXeVJqWIQ7gW9tY1GJIpUtFb6CmjVyq2VM3u71bOyR8CRihcCgMUYoDNyLXao3+70Q=="
                } else {
                    "195e2be3172d7684bf95cff69ae3b7a15a9841ea9d27d3c843662d50cdd7d6470fd9c8e64be84d031117e4a4083486effba39f9aef6bbb2c89f7f21bcfba33ba"
                }
            }
        );

        assert_eq!(
            res.1.last().unwrap(),
            &Entry {
                name: "yargs",
                version: "17.5.1",
                descriptors: vec![("yargs", "^17.5.1")],
                dependencies: vec![
                    ("cliui", "^7.0.2"),
                    ("escalade", "^3.1.1"),
                    ("get-caller-file", "^2.0.5"),
                    ("require-directory", "^2.1.1"),
                    ("string-width", "^4.2.3"),
                    ("y18n", "^5.0.5"),
                    ("yargs-parser", "^21.0.0"),
                ],
                integrity: if with_bug {
                    "sha512-t6YAJcxDkNX7NFYiVtKvWUz8l+PaKTLiL63mJYWR2GnHq2gjEWISzsLp9wg3aY36dY1j+gfIEL3pIF+XlJJfbA=="
                } else {
                    "00d58a2c052937fa044834313f07910fd0a115dec5ee35919e857eeee3736b21a4eafa8264535800ba8bac312991ce785ecb8a51f4d2cc8c4676d865af1cfbde"
                }
            }
        );
    }

    #[test]
    fn parse_v6_doc_from_file_works() {
        let content = std::fs::read_to_string("tests/v2/yarn.lock").unwrap();
        let res = parse(&content).unwrap();
        assert_v6(res, false)
    }

    #[test]
    fn parse_v6_doc_from_file_without_endline_works() {
        let content = std::fs::read_to_string("tests/v2_without_endline/yarn.lock").unwrap();
        let res = parse(&content).unwrap();
        assert_v6(res, false)
    }

    #[test]
    fn parse_v6_doc_from_memory_works() {
        fn assert(input: &str, expect: &[Entry]) {
            let res = parse(input).unwrap();
            assert_eq!(res.1, expect);
        }

        assert(
            r#"# This file is generated by running "yarn install" inside your project.
# Manual changes might be lost - proceed with caution!

__metadata:
  version: 6
  cacheKey: 8

"@babel/helper-plugin-utils@npm:^7.16.7":
  version: 7.16.7
  resolution: "@babel/helper-plugin-utils@npm:7.16.7"
  checksum: d08dd86554a186c2538547cd537552e4029f704994a9201d41d82015c10ed7f58f9036e8d1527c3760f042409163269d308b0b3706589039c5f1884619c6d4ce
  languageName: node
  linkType: hard

"@babel/plugin-transform-for-of@npm:^7.12.1":
  version: 7.16.7
  resolution: "@babel/plugin-transform-for-of@npm:7.16.7"
  dependencies:
    "@babel/helper-plugin-utils": ^7.16.7
  peerDependencies:
    "@babel/core": ^7.0.0-0
  checksum: 35c9264ee4bef814818123d70afe8b2f0a85753a0a9dc7b73f93a71cadc5d7de852f1a3e300a7c69a491705805704611de1e2ccceb5686f7828d6bca2e5a7306
  languageName: node
  linkType: hard

"@babel/runtime@npm:^7.12.5":
  version: 7.17.9
  resolution: "@babel/runtime@npm:7.17.9"
  dependencies:
    regenerator-runtime: ^0.13.4
  checksum: 4d56bdb82890f386d5a57c40ef985a0ed7f0a78f789377a2d0c3e8826819e0f7f16ba0fe906d9b2241c5f7ca56630ef0653f5bb99f03771f7b87ff8af4bf5fe3
  languageName: node
  linkType: hard
"#,
            &[
                Entry {
                    name: "@babel/helper-plugin-utils",
                    version: "7.16.7",
                    descriptors: vec![("@babel/helper-plugin-utils", "^7.16.7")],
                    integrity: "d08dd86554a186c2538547cd537552e4029f704994a9201d41d82015c10ed7f58f9036e8d1527c3760f042409163269d308b0b3706589039c5f1884619c6d4ce",
                    ..Default::default()
                },
                Entry {
                    name: "@babel/plugin-transform-for-of",
                    version: "7.16.7",
                    descriptors: vec![("@babel/plugin-transform-for-of", "^7.12.1")],
                    dependencies: vec![("@babel/helper-plugin-utils", "^7.16.7")],
                    integrity: "35c9264ee4bef814818123d70afe8b2f0a85753a0a9dc7b73f93a71cadc5d7de852f1a3e300a7c69a491705805704611de1e2ccceb5686f7828d6bca2e5a7306",
                },
                Entry {
                    name: "@babel/runtime",
                    version: "7.17.9",
                    descriptors: vec![("@babel/runtime", "^7.12.5")],
                    dependencies: vec![("regenerator-runtime", "^0.13.4")],
                    integrity: "4d56bdb82890f386d5a57c40ef985a0ed7f0a78f789377a2d0c3e8826819e0f7f16ba0fe906d9b2241c5f7ca56630ef0653f5bb99f03771f7b87ff8af4bf5fe3"
                },
            ],
        );
    }

    #[test]
    fn parse_v6_doc_from_memory_with_npm_in_dependencies_works() {
        fn assert(input: &str, expect: &[Entry]) {
            let res = parse(input).unwrap();
            assert_eq!(res.1, expect);
        }

        assert(
            r#"# This file is generated by running "yarn install" inside your project.
# Manual changes might be lost - proceed with caution!

__metadata:
  version: 6
  cacheKey: 8

"foo@workspace:.":
  version: 0.0.0-use.local
  resolution: "foo@workspace:."
  dependencies:
    valib-aliased: "npm:valib@1.0.0 || 1.0.1"
  languageName: unknown
  linkType: soft

"valib-aliased@npm:valib@1.0.0 || 1.0.1":
  version: 1.0.0
  resolution: "valib@npm:1.0.0"
  checksum: ad4f5a0b5dde5ab5e3cc87050fad4d7096c32797454d8e37c7dadf3455a43a7221a3caaa0ad9e72b8cd96668168e5a25d5f0072e21990f7f80a64b1a4e34e921
  languageName: node
  linkType: hard
"#,
            &[
                Entry {
                    name: "foo",
                    version: "0.0.0-use.local",
                    integrity: "",
                    descriptors: vec![("foo", ".")],
                    dependencies: vec![("valib-aliased", "1.0.0 || 1.0.1")],
                },
                Entry {
                    name: "valib-aliased",
                    version: "1.0.0",
                    integrity: "ad4f5a0b5dde5ab5e3cc87050fad4d7096c32797454d8e37c7dadf3455a43a7221a3caaa0ad9e72b8cd96668168e5a25d5f0072e21990f7f80a64b1a4e34e921",
                    descriptors: vec![("valib-aliased", "1.0.0 || 1.0.1")],
                    dependencies: vec![],
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
            "\"@babel/code-frame@^7.0.0\":\r\n  version \"7.12.13\"\r\n  resolved \"https://registry.yarnpkg.com/@babel/code-frame/-/code-frame-7.12.13.tgz#dcfc826beef65e75c50e21d3837d7d95798dd658\"\r\n  integrity sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g==\r\n  dependencies:\r\n    \"@babel/highlight\" \"^7.12.13\"\r\n\r\n",
            Entry {
                name: "@babel/code-frame",
                version: "7.12.13",
                descriptors: vec![("@babel/code-frame", "^7.0.0")],
                dependencies: vec![("@babel/highlight", "^7.12.13")],
                integrity: "sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g==",
                ..Default::default()
            },
        );

        assert(
                    "\"@babel/code-frame@^7.0.0\":\n  version \"7.12.13\"\n  resolved \"https://registry.yarnpkg.com/@babel/code-frame/-/code-frame-7.12.13.tgz#dcfc826beef65e75c50e21d3837d7d95798dd658\"\n  integrity sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g==\n  dependencies:\n    \"@babel/highlight\" \"^7.12.13\"\n\n",
                    Entry {
                        name: "@babel/code-frame",
                        version: "7.12.13",
                        descriptors: vec![("@babel/code-frame", "^7.0.0")],
                        dependencies: vec![("@babel/highlight", "^7.12.13")],
                        integrity: "sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g==",
                        ..Default::default()
                    },
                );

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
                        integrity: "sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g=="
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
                        integrity: "sha512-np/lG3uARFybkoHokJUmf1QfEvRVCPbmQeUQpKow5cQ3xWrV9i3rUHodKDJPQfTVX61qKi+UdYk8kik84n7XOw==",
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
                        integrity: "sha512-np/lG3uARFybkoHokJUmf1QfEvRVCPbmQeUQpKow5cQ3xWrV9i3rUHodKDJPQfTVX61qKi+UdYk8kik84n7XOw==",
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
        // escaped lines
        assert(
            "\"@babel/code-frame@^7.0.0\":\n  version \"7.12.13\"\n  resolved \"https://registry.yarnpkg.com/@babel/code-frame/-/code-frame-7.12.13.tgz#dcfc826beef65e75c50e21d3837d7d95798dd658\"\n  integrity sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g==\n  dependencies:\n    \"@babel/highlight\" \"^7.12.13\"\n\n",
            Entry {
                name: "@babel/code-frame",
                version: "7.12.13",
                descriptors: vec![("@babel/code-frame", "^7.0.0")],
                dependencies: vec![("@babel/highlight", "^7.12.13")],
                integrity: "sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g==",
                ..Default::default()
            },
        );

        // escaped lines windows
        assert(
            "\"@babel/code-frame@^7.0.0\":\r\n  version \"7.12.13\"\r\n  resolved \"https://registry.yarnpkg.com/@babel/code-frame/-/code-frame-7.12.13.tgz#dcfc826beef65e75c50e21d3837d7d95798dd658\"\r\n  integrity sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g==\r\n  dependencies:\r\n    \"@babel/highlight\" \"^7.12.13\"\r\n\r\n",
            Entry {
                name: "@babel/code-frame",
                version: "7.12.13",
                descriptors: vec![("@babel/code-frame", "^7.0.0")],
                dependencies: vec![("@babel/highlight", "^7.12.13")],
                integrity: "sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g==",
                ..Default::default()
            },
        );

        // normal
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
                        integrity: "sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g==",
                        ..Default::default()
                    },
                );
    }

    #[test]
    fn parse_entry_without_endline_works() {
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
        "@babel/highlight" "^7.12.13""#,
            Entry {
                name: "@babel/code-frame",
                version: "7.12.13",
                descriptors: vec![("@babel/code-frame", "^7.0.0")],
                dependencies: vec![("@babel/highlight", "^7.12.13")],
                integrity: "sha512-HV1Cm0Q3ZrpCR93tkWOYiuYIgLxZXZFVG2VgK+MBWjUqZTundupbfx2aXarXuw5Ko5aMcjtJgbSs4vUGBS5v6g==",
                ..Default::default()
            },
        );
    }

    #[test]
    fn entry_version_works() {
        assert_eq!(
            entry_version(" version \"1.2.3\"\r\n"),
            Ok(("", EntryItem::Version("1.2.3")))
        );
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

        assert(
            r#"foolib@npm:1.2.3 || ^2.0.0":
            version "1.2.3"
        "#,
            vec![("foolib", "1.2.3 || ^2.0.0")],
        );
    }

    #[test]
    fn unknown_line_works() {
        let res = unknown_line("foo\nbar").unwrap();
        assert_eq!(res, ("bar", EntryItem::Unknown("foo\n")));
    }

    #[test]
    fn integrity_works() {
        fn assert(input: &str, expect: EntryItem) {
            let res = integrity(input).unwrap();
            assert_eq!(res.1, expect);
        }

        assert(
            r#" "integrity" "sha1-jQrELxbqVd69MyyvTEA4s+P139k="
        "#,
            EntryItem::Integrity("sha1-jQrELxbqVd69MyyvTEA4s+P139k="),
        );

        assert(
            r#" integrity sha1-jQrELxbqVd69MyyvTEA4s+P139k=
        "#,
            EntryItem::Integrity("sha1-jQrELxbqVd69MyyvTEA4s+P139k="),
        );

        assert(
            r#" checksum: fb47e70bf0001fdeabdc0429d431863e9475e7e43ea5f94ad86503d918423c1543361cc5166d713eaa7029dd7a3d34775af04764bebff99ef413111a5af18c80
        "#,
            EntryItem::Integrity("fb47e70bf0001fdeabdc0429d431863e9475e7e43ea5f94ad86503d918423c1543361cc5166d713eaa7029dd7a3d34775af04764bebff99ef413111a5af18c80"),
        );
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

        assert(
            r#"            dependencies:
                foo "1.0 || 2.0"
                "bar" "0.3-alpha1"
        "#,
            EntryItem::Dependencies(vec![("foo", "1.0 || 2.0"), ("bar", "0.3-alpha1")]),
        );

        assert(
            r#"            dependencies:
                foo: 1.0 || 2.0
                "bar": "0.3-alpha1"
        "#,
            EntryItem::Dependencies(vec![("foo", "1.0 || 2.0"), ("bar", "0.3-alpha1")]),
        );
    }

    #[test]
    fn take_till_the_end_works() {
        let k = take_till_line_end("foo\r\nbar").unwrap();
        assert_eq!(k.0, "bar");
        assert_eq!(k.1, "foo\r\n");
    }
}
