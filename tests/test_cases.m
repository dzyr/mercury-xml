%-----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% Author: Dirk Ziegemeyer <dirk@ziegemeyer.de>
%
%-----------------------------------------------------------------------------%
% Any copyright is dedicated to the Public Domain.
% http://creativecommons.org/publicdomain/zero/1.0/
%-----------------------------------------------------------------------------%

:- module test_cases.
:- interface.

:- import_module test_utils.
:- import_module xml_read.

:- import_module list.
:- import_module maybe.
:- import_module stream.


:- func get_tests_foldl_content = list(test_case(
    stream.maybe_partial_res(list(content_event), xml_error))).


:- func get_tests_get_xml_declaration = list(test_case(ok_error(xml_error))).


:- func get_test_example = list(test_case(
    maybe_error(list(content_event), xml_error))).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module pair.


get_tests_foldl_content = [
    test_case(
        "empty_elem_tag1",
        ok([
            elem_etag(elem_name("EmptyElemTag")),
            elem_stag(elem_name("EmptyElemTag"), [])
        ])
    ),
    test_case(
        "empty_elem_tag2",
        ok([
            elem_etag(elem_name("EmptyElemTag")),
            elem_stag(elem_name("EmptyElemTag"), [])
        ])
    ),
    test_case(
        "empty_elem_tag3",
        ok([
            elem_etag(elem_name("EmptyElemTag")),
            elem_stag(elem_name("EmptyElemTag"), [])
        ])
    ),
    test_case(
        "empty_elem_tag4",
        ok([
            elem_etag(elem_name("EmptyElemTag")),
            elem_stag(elem_name("EmptyElemTag"), [
                att_name("ATTRIBUTE_A") - att_value("Value-1")
            ])
        ])
    ),
    test_case(
        "empty_elem_tag5",
        ok([
            elem_etag(elem_name("EmptyElemTag")),
            elem_stag(elem_name("EmptyElemTag"), [
                att_name("ATTRIBUTE_B") - att_value("Value-2"),
                att_name("ATTRIBUTE_A") - att_value("Value-1")
            ])
        ])
    ),
    test_case(
        "empty_elem_tag6",
        ok([
            elem_etag(elem_name("EmptyElemTag")),
            elem_stag(elem_name("EmptyElemTag"), [
                att_name("A:-1234567890.abc") - att_value("Value-1")
            ])
        ])
    ),
    test_case(
        "empty_elem_tag7",
        ok([
            elem_etag(elem_name("EmptyElemTag")),
            elem_stag(elem_name("EmptyElemTag"), [
                att_name("C:-1234567890.abc") - att_value("Value-3"),
                att_name("B:-1234567890.abc") - att_value("Value-2"),
                att_name("A:-1234567890.abc") - att_value("Value-1")
            ])
        ])
    ),
    test_case(
        "empty_elem_tags1",
        ok([
            elem_etag(elem_name("EmptyElemTag")),
            elem_stag(elem_name("EmptyElemTag"), [
                att_name("AttName") - att_value("AttValue")
            ]),
            elem_etag(elem_name("EmptyElemTag")),
            elem_stag(elem_name("EmptyElemTag"), []),
            elem_etag(elem_name("EmptyElemTag")),
            elem_stag(elem_name("EmptyElemTag"), [
                att_name("AttName1") - att_value("AttValue1")
            ]),
            elem_etag(elem_name("EmptyElemTag")),
            elem_stag(elem_name("EmptyElemTag"), [])
        ])
    ),
    test_case(
        "stag1",
        ok([
            elem_stag(elem_name("STag"), [])
        ])
    ),
    test_case(
        "stag2",
        ok([
            elem_stag(elem_name("STag"), [])
        ])
    ),
    test_case(
        "stag3",
        ok([
            elem_stag(elem_name("STag"), [])
        ])
    ),
    test_case(
        "stag4",
        ok([
            elem_stag(elem_name("STag"), [
                att_name("Att1") - att_value("&-'->-<-\"")
            ])
        ])
    ),
    test_case(
        "stag5",
        ok([
            elem_stag(elem_name("STag"), [
                att_name("Att1") - att_value("AttValue")
            ])
        ])
    ),
    test_case(
        "stag6",
        ok([
            elem_stag(elem_name("STag"), [
                att_name("Att1") - att_value("ValueInsideSingleQuotes")
            ])
        ])
    ),
    test_case(
        "etag1",
        ok([
            elem_etag(elem_name("ETag"))
        ])
    ),
    test_case(
        "etag2",
        ok([
            elem_etag(elem_name("ETag"))
        ])
    ),
    test_case(
        "stag_etag_empty_elem_tag",
        ok([
            elem_etag(elem_name("Tag")),
            elem_etag(elem_name("EmptyElemTag")),
            elem_stag(elem_name("EmptyElemTag"), [
                att_name("Att2Name") - att_value("Att2Value"),
                att_name("Att1Name") - att_value("Att1Value")
            ]),
            elem_etag(elem_name("EmptyElemTag")),
            elem_stag(elem_name("EmptyElemTag"), [
                att_name("AttName") - att_value("AttValue")
            ]),
            elem_stag(elem_name("Tag"), [
                att_name("TagAttName") - att_value("TagAttValue")
            ])
        ])
    ),
    test_case(
        "textual_data1",
        ok([
            elem_etag(elem_name("Tag")),
            data("Textual data"),
            elem_stag(elem_name("Tag"), [
                att_name("Att1") - att_value("Value1")
            ])
        ])
    ),
    test_case(
        "textual_data2",
        ok([
            elem_etag(elem_name("OtherTag")),
            data("other textual data  "),
            elem_stag(elem_name("OtherTag"), [
                att_name("OAtt1") - att_value("OValue1")
            ]),
            elem_etag(elem_name("Tag")),
            data("Textual data"),
            elem_stag(elem_name("Tag"), [
                att_name("Att1") - att_value("Value1")
            ])
        ])
    )
].

%-----------------------------------------------------------------------------%

get_tests_get_xml_declaration = [
    test_case(
        "xml_decl1",
        ok
    )
].

%-----------------------------------------------------------------------------%

get_test_example = [
    test_case(
        "example",
        ok([
            elem_stag(elem_name("doc"), []),
            elem_stag(elem_name("Element"), [
                att_name("Attribute") - att_value("Value")
            ]),
            data("Text"),
            elem_etag(elem_name("Element")),
            elem_etag(elem_name("doc"))
        ])
    )
].
