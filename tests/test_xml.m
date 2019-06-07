%-----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% Author: Dirk Ziegemeyer <dirk@ziegemeyer.de>
%
%-----------------------------------------------------------------------------%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%-----------------------------------------------------------------------------%

:- module test_xml.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module test_cases.
:- import_module test_utils.
:- import_module xml_read.

:- import_module benchmarking.
:- import_module bool.
:- import_module char.
:- import_module getopt.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module stream.
:- import_module string.
:- import_module term_to_xml.
:- import_module uint.


main(!IO) :-
    det_read_options(Options, !IO),
    FileName = "large.xml",
    (
        Options = help,
        help(!IO)
    ;
        Options = benchmark,
        io.write_string("start benchmarking\n", !IO),
        io.open_input(FileName, InputResult, !IO),
        (
            InputResult = ok(Stream),
            promise_equivalent_solutions [EventsResult, Time, !:IO] (
                benchmark_det_io(test_fold_content_helper, Stream,
                    EventsResult, !IO, 1, Time)
            ),
            (
                EventsResult = ok(_),
                io.format("%d ms\n", [i(Time)], !IO)
            ;
                EventsResult = error(_, _),
                require.error("error during benchmarking")
            )
        ;
            InputResult = error(IO_Error),
            report_file_open_error(FileName, IO_Error, !IO)
        )
    ;
        Options = test(Verbosity),
        run_tests(Verbosity, !IO)
    ;
        Options = write_large_file,
        io.write_string("write large test file\n", !IO),
        io.open_output(FileName, Result, !IO),
        (
            Result = ok(Stream),
            term_to_xml.write_xml_element(Stream, 0,
                large_xml_document(300000u), !IO),
            io.close_output(Stream, !IO)
        ;
            Result = error(Code),
            require.error(string.format(
                "error while trying to write to file %s: %s",
                [s(FileName), s(io.error_message(Code))]))
        )
    ).

%-----------------------------------------------------------------------------%

:- pred run_tests(verbosity::in, io::di, io::uo) is det.

run_tests(Verbosity, !IO) :-
    run_tests2(Verbosity, init_test_results, Results, !IO),
    print_results(Results, !IO).


:- pred run_tests2(verbosity::in, test_results::in, test_results::out,
    io::di, io::uo) is det.

run_tests2(Verbosity, !Results, !IO) :-
    % Run tests: xml_read.fold_content
    list.foldl2(
        run_test_stream(test_fold_content_helper, Verbosity),
        get_tests_foldl_content, !Results, !IO),


    % Run tests: xml_read.get_xml_declaration
    list.foldl2(
        run_test_stream(xml_read.get_xml_declaration, Verbosity),
        get_tests_get_xml_declaration, !Results, !IO),


    % Run tests: example
    list.foldl2(
        run_test_stream(read_example_from_stream, Verbosity),
        get_test_example, !Results, !IO).

%-----------------------------------------------------------------------------%

:- pred test_fold_content_helper(io.input_stream::in,
    stream.maybe_partial_res(list(content_event), xml_error)::out,
    io::di, io::uo) is det.

test_fold_content_helper(Stream, ActualResult, !IO) :-
    xml_read.fold_content(list.cons, Stream, [], ActualResult, !IO).


%-----------------------------------------------------------------------------%

:- pred read_example_from_stream(io.input_stream::in,
    maybe_error(list(content_event), xml_error)::out,
    io::di, io::uo) is det.

read_example_from_stream(Stream, X, !IO) :-
    get_xml_declaration(Stream, XmlDeclResult, !IO),
    (
        XmlDeclResult = ok,
        xml_read.fold_content(list.cons, Stream, [], ContentResult, !IO),
        (
            ContentResult = ok(XmlEvents),
            X = ok(list.reverse(XmlEvents))
        ;
            ContentResult = error(_PartialRes, ContentError),
            X = error(ContentError)
        )
    ;
        XmlDeclResult = error(XmlDeclError),
        X = error(XmlDeclError)
    ).

%-----------------------------------------------------------------------------%
%
% Command line options.
%

:- type option
    --->    help
    ;       benchmark
    ;       verbose
    ;       write_large_file.


:- pred short_option(char, option).
:- mode short_option(in, out) is semidet.
:- mode short_option(out, in) is det.

short_option('h', help).
short_option('b', benchmark).
short_option('v', verbose).
short_option('w', write_large_file).


:- pred long_option(string::in, option::out) is semidet.

long_option("help", help).
long_option("benchmark", benchmark).
long_option("verbose", verbose).
long_option("write-large-file", write_large_file).


:- pred option_default(option, option_data).
:- mode option_default(in, out) is det.
:- mode option_default(out, out) is multi.

option_default(help, bool(no)).
option_default(benchmark, bool(no)).
option_default(verbose, bool(no)).
option_default(write_large_file, bool(no)).

%-----------------------------------------------------------------------------%

:- type processing_options
    --->    help
    ;       write_large_file
    ;       benchmark
    ;       test(verbosity).


    % Get processing options. Abort with error in case of invalid options.
    %
:- pred det_read_options(processing_options::out,
    io::di, io::uo) is det.

det_read_options(X, !IO) :-
    io.command_line_arguments(Args, !IO),
    OptionOps = option_ops_multi(
        (pred(C::in, O::out) is semidet :- short_option(C, O)),
        long_option,
        (pred(O::out, D::out) is multi :- option_default(O, D))
    ),
    getopt.process_options(OptionOps, Args, _NonOptionArgs, GetoptResult),
    (
        GetoptResult = ok(OptionTable),
        getopt.lookup_bool_option(OptionTable, help, Help),
        (
            Help = yes,
            X = help
        ;
            Help = no,
            getopt.lookup_bool_option(OptionTable, write_large_file, Large),
            (
                Large = no,
                getopt.lookup_bool_option(OptionTable, benchmark, Speed),
                (
                    Speed = yes,
                    X = benchmark
                ;
                    Speed = no,
                    X = test(get_verbosity(OptionTable))
                )
            ;
                Large = yes,
                X = write_large_file
            )
        )
    ;
        GetoptResult = error(Msg),
        require.error(string.format(
            "test_xml: %s\ntest_xml: use --help for more information.\n",
            [s(Msg)]))
    ).

%-----------------------------------------------------------------------------%

:- func get_verbosity(option_table(option)) = verbosity.

get_verbosity(OptionTable) = X :-
    getopt.lookup_bool_option(OptionTable, verbose, Verbose),
    (
        Verbose = yes,
        X = verbose
    ;
        Verbose = no,
        X = silent
    ).

%-----------------------------------------------------------------------------%

:- pred help(io::di, io::uo) is det.

help(!IO) :-
    io.write_string("Usage: test_xml [<options>]\n", !IO),
    io.write_strings([
        "Options:\n",
        "\t-h, --help\n",
        "\t\tPrint this message.\n",

        "\t-v, --verbose\n",
        "\t\tOutput progress information.\n",

        "\t-b, --benchmark\n",
        "\t\tPerform benchmark test.\n",

        "\t-w, --write-large-file\n",
        "\t\tWrite a large file to disk for benchmarking\n"
    ], !IO).

%-----------------------------------------------------------------------------%

:- type large_xml_document
    --->    large_xml_document(uint).

:- instance xmlable(large_xml_document) where [
    func(to_xml/1) is get_parent_elem
].


:- func get_parent_elem(large_xml_document::in) = (xml::out(xml_doc)) is det.

get_parent_elem(large_xml_document(X)) =
    elem(
        "ParentElemName",
        get_attributes,
        acc_until(X, get_empty_elem, [])
    ).


:- func get_empty_elem = (xml::out(xml_doc)) is det.

get_empty_elem =
    elem(
        "ElemName",
        get_attributes,
        []
    ).


:- func get_attributes = list(attr).

get_attributes = [
    attr("ATTRIBUTE1", "VALUE1"),
    attr("ATTRIBUTE2", "VALUE2"),
    attr("ATTRIBUTE3", "VALUE-&amp;-3"),
    attr("ATTRIBUTE4", "VALUE4"),
    attr("ATTRIBUTE5", "VALUE5"),
    attr("ATTRIBUTE6", "VALUE6"),
    attr("ATTRIBUTE7", "VALUE7"),
    attr("ATTRIBUTE8", "VALUE8"),
    attr("ATTRIBUTE9", "VALUE9"),
    attr("ATTRIBUTE10", "VALUE10"),
    attr("ATTRIBUTE11", "VALUE11"),
    attr("ATTRIBUTE12", "VALUE12"),
    attr("ATTRIBUTE13", "VALUE13"),
    attr("ATTRIBUTE14", "VALUE14"),
    attr("ATTRIBUTE15", "VALUE15")
].


    % acc_until(N, T, Acc0) adds T n times to Acc0
    %
:- func acc_until(uint, T, list(T)) = list(T).

acc_until(Counter, X, Acc0) = Acc :-
    ( if Counter = 0u then
        Acc = Acc0
    else
        Acc = acc_until(Counter - 1u, X, [X | Acc0])
    ).
