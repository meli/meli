#!python3
#
# meli - scripts/markdown_doc_lints.py
#
# Copyright 2024 Manos Pitsidianakis
#
# This file is part of meli.
#
# meli is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# meli is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with meli. If not, see <http://www.gnu.org/licenses/>.
#
# SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

""" Format and validate with:

```
black scripts/markdown_doc_lints.py
pylint scripts/markdown_doc_lints.py
mypy --strict-equality -strict scripts/markdown_doc_lints.py
```
"""

# pylint: disable=invalid-name, redefined-outer-name

from typing import Mapping, Any, Tuple
import io
import tomllib
import argparse
import sys
import warnings
import re


def validate_cargo_toml(cargo_toml: io.BufferedReader) -> Mapping[str, Any]:
    """Parse Cargo.toml contents and return it as a dictionary"""
    try:
        cargo_toml_dict: Mapping[str, Any] = tomllib.load(cargo_toml)
    except tomllib.TOMLDecodeError as exc:
        print(cargo_toml.name, "could not be parsed as TOML")
        raise exc

    if "features" not in cargo_toml_dict:
        print(
            "`features` key not found in",
            cargo_toml.name,
            "is it the correct Cargo.toml you wanted to pass?",
        )
        sys.exit(1)

    return cargo_toml_dict


def validate_default_features(
    readme_md: io.TextIOWrapper, readme_md_contents: str, features: dict[str, list[str]]
) -> bool:
    """Ensure README.md has a part mentioning the default features in a toml block"""
    default_features_md_paragraph_re = r"```toml\ndefault\s*=.*?\n```"
    if "default" in features:
        default_features_list: list[str] = list(features["default"])
        default_feature_string = """```toml
default = ["""
        for f in default_features_list:
            if not default_feature_string.endswith(" ["):
                default_feature_string += ", "
            default_feature_string += f'"{f}"'
        default_feature_string += """]
```
"""
        if default_feature_string not in readme_md_contents:
            existing_match = re.search(
                default_features_md_paragraph_re, readme_md_contents, re.IGNORECASE
            )
            if existing_match is None:
                print(
                    "A paragraph mentioning the default contents must be present in",
                    readme_md.name,
                )
                print("Add the following markdown:")
            else:
                print(
                    "The paragraph in",
                    readme_md.name,
                    "mentioning the default contents has invalid or incorrectly formatted contents",
                )
                print("What exists in", readme_md.name, "is:")
                print(existing_match.group(0))
                print("Replace it with the following markdown:")
            print(default_feature_string, end="")
            return False
    return True


def validate_valid_features_mentioned(
    cargo_toml: io.BufferedReader,
    readme_md: io.TextIOWrapper,
    readme_md_contents: str,
    features_list: list[str],
) -> bool:
    """Ensure that valid features as parsed from Cargo.toml manifest are
    mentioned in provided README.md with the correct format (a named HTML anchor element)
    """
    missing: list[Tuple[str, str]] = []
    for feat in features_list:
        anchor_fmt = f'<a name="{feat}-feature">`{feat}`</a>'
        if anchor_fmt not in readme_md_contents:
            missing.append((feat, anchor_fmt))
    if len(missing) == len(features_list):
        print("None of the features in", cargo_toml.name, "appear in", readme_md.name)
        print(f"{features_list=}")
        return False
    if len(missing) > 0:
        print(
            "The following",
            len(missing),
            "feature" if len(missing) == 1 else "features",
            "are not mentioned in",
            readme_md.name,
        )
        print("")
        for m, anchor in missing:
            print("-", m, "add the following string in the Features section:", anchor)
        return False
    return True


def validate_readme_feature_named_anchors(
    cargo_toml: io.BufferedReader,
    readme_md_contents: str,
    features_list: list[str],
) -> bool:
    """Ensure name attribute of anchor HTML elements match the feature name,
    and also that all such elements refer to valid features
    (as parsed from the Cargo.toml manifest)"""
    anchor_re = (
        r"[<]a name=[\"](?P<name_attr>.+)-feature[\"][>][`](?P<name>[^`]+)[`][<].a[>]"
    )
    matches = re.finditer(anchor_re, readme_md_contents)
    newline_re = re.compile(r"\n")
    retval = True
    for match in matches:
        if match.group("name_attr") != match.group("name"):
            start_line = (
                len(newline_re.findall(readme_md_contents, 0, match.start("name_attr")))
                + 1
            )
            end_line = (
                len(newline_re.findall(readme_md_contents, 0, match.end("name_attr")))
                + 1
            )
            if start_line == end_line:
                print("In line ", start_line, ",", end="", sep="")
            else:
                print("In lines ", start_line, ":", end_line, ",", end="", sep="")
            print(
                " attribute name `",
                match.group("name_attr"),
                "` does not match feature name `",
                match.group("name"),
                "`",
                sep="",
            )
            retval = False
        for feature_name in ["name_attr", "name"]:
            if feature_name == "name" and match.group(feature_name) == match.group(
                "name_attr"
            ):
                continue
            if (
                len(match.group(feature_name)) > 0
                and match.group(feature_name) not in features_list
            ):
                start_line = (
                    len(
                        newline_re.findall(
                            readme_md_contents, 0, match.start(feature_name)
                        )
                    )
                    + 1
                )
                end_line = (
                    len(
                        newline_re.findall(
                            readme_md_contents, 0, match.end(feature_name)
                        )
                    )
                    + 1
                )
                if start_line == end_line:
                    print("In line ", start_line, ",", end="", sep="")
                else:
                    print("In lines ", start_line, ":", end_line, ",", end="", sep="")
                print(
                    " feature `",
                    match.group(feature_name),
                    "` is mentioned but it does not appear in the features defined in ",
                    cargo_toml.name,
                    sep="",
                )
                retval = False
    return retval


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Parses a README.md and the crate's Cargo.toml"
        "to ensure all compile-time features are documented. Exits with 1 if an error was found."
        "\n"
        "Example usage:\n"
        "python3 scripts/markdown_doc_lints.py melib/README.md melib/Cargo.toml"
    )
    parser.add_argument("README.md", type=argparse.FileType("r"), help="README.md file")
    parser.add_argument(
        "Cargo.toml", type=argparse.FileType("rb"), help="Cargo.toml manifest file"
    )
    args = parser.parse_args()

    readme_md: io.TextIOWrapper = vars(args)["README.md"]
    cargo_toml: io.BufferedReader = vars(args)["Cargo.toml"]

    if not readme_md.name.endswith("README.md"):
        warnings.warn(
            "The file given as the README.md argument is not named README.md. Was this a mistake?"
        )

    readme_md_contents: str = vars(args)["README.md"].read()

    cargo_toml_dict: Mapping[str, Any] = validate_cargo_toml(cargo_toml)

    features: dict[str, list[str]] = cargo_toml_dict["features"]

    success = True

    success &= validate_default_features(readme_md, readme_md_contents, features)

    for to_remove in ["default", "debug-tracing"]:
        if to_remove in features:
            del features[to_remove]

    features_list: list[str] = list(features)

    success &= validate_valid_features_mentioned(
        cargo_toml, readme_md, readme_md_contents, features_list
    )

    success &= validate_readme_feature_named_anchors(
        cargo_toml, readme_md_contents, features_list
    )

    sys.exit(0 if success else 1)
