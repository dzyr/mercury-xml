%-----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% Author: Dirk Ziegemeyer <dirk@ziegemeyer.de>
%
% Including code from https://github.com/juliensf/mercury-csv/test_csv by
% Julien Fischer licensed under 2-clause BSD style license.
%
%-----------------------------------------------------------------------------%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%-----------------------------------------------------------------------------%

:- module test_utils.
:- interface.


:- import_module io.

%-----------------------------------------------------------------------------%
% Code in this section is based on the module
% https://github.com/juliensf/mercury-csv/test_csv
% by Julien Fischer, licensed under 2-clause BSD style license.
%-----------------------------------------------------------------------------%

:- type test_results.


:- func init_test_results = test_results.


:- pred report_file_open_error(string::in, io.error::in,
    io::di, io::uo) is det.


:- pred print_results(test_results::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
% Author of this section: Dirk Ziegemeyer
%-----------------------------------------------------------------------------%

:- type test_case(T)
    --->    test_case(
                test_name       :: string,  % without extension ".inp"
                test_expected   :: T
            ).


:- type verbosity
    --->    silent
    ;       verbose.


:- pred run_test_stream(
    pred(io.input_stream, T, io, io)::in(pred(in, out, di, uo) is det),
    verbosity::in, test_case(T)::in,
    test_results::in, test_results::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.


:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

run_test_stream(StreamToResultPred, Verbosity, TestCase, !Results, !IO) :-
    increment_total_tests(!Results),
    maybe_verbose(Verbosity,
        io.format("RUNNING TEST: %s ... ",
            [s(TestCase^test_name)]),
        !IO),

    InputFileName = TestCase^test_name ++ ".inp",
    io.open_input(InputFileName, InputResult, !IO),
    (
        InputResult = ok(Stream),
        StreamToResultPred(Stream, ActualResult, !IO),
        compare(Verbosity,
            TestCase^test_name,
            TestCase^test_expected, ActualResult, !Results, !IO)
    ;
        InputResult = error(IO_Error),
        add_aborted_test(TestCase^test_name, !Results),
        report_file_open_error(InputFileName, IO_Error, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred compare(verbosity::in, string::in, T::in, T::in,
    test_results::in, test_results::out, io::di, io::uo) is det.

compare(Verbosity, Name, Expected, Result, !Results, !IO) :-
    ( if Result = Expected
    then maybe_verbose(Verbosity, io.write_string("PASSED. "), !IO)
    else
        add_failed_test(Name, !Results),
        maybe_verbose(Verbosity, io.write_string("FAILED. "), !IO)
    ),
    maybe_verbose(Verbosity, io.print(Result), !IO),
    maybe_verbose(Verbosity, nl, !IO).

%-----------------------------------------------------------------------------%

:- pred maybe_verbose(verbosity::in,
    pred(io, io)::in(pred(di, uo) is det), io::di, io::uo) is det.

maybe_verbose(Verbose, Pred, !IO) :-
    (
        Verbose = verbose,
        Pred(!IO)
    ;
        Verbose = silent
    ).

%-----------------------------------------------------------------------------%
% Code in this section is based on the module
% https://github.com/juliensf/mercury-csv/test_csv
% by Julien Fischer, licensed under 2-clause BSD style license.
%-----------------------------------------------------------------------------%

:- type test_results
    --->    test_results(
                total_tests  :: int,

                failed_tests :: list(string),
                % Tests that failed.

                aborted_tests :: list(string)
                % Tests that caused an error.
            ).

init_test_results = test_results(0, [], []).


:- pred increment_total_tests(test_results::in, test_results::out) is det.

increment_total_tests(!Results) :-
    !Results ^ total_tests := !.Results ^ total_tests + 1.


:- pred add_failed_test(string::in,
    test_results::in, test_results::out) is det.

add_failed_test(Name, !Results) :-
    !Results ^ failed_tests := [Name | !.Results ^ failed_tests].


:- pred add_aborted_test(string::in,
    test_results::in, test_results::out) is det.

add_aborted_test(Name, !Results) :-
    !Results ^ aborted_tests := [Name | !.Results ^ aborted_tests].


print_results(Results, !IO) :-
    Results = test_results(_Total, Failed, Aborted),
    list.length(Failed, NumFailed),
    list.length(Aborted, NumAborted),
    ( if NumFailed = 0, NumAborted = 0 then
        io.write_string("ALL TESTS PASSED\n", !IO),
        io.remove_file("FAILED_TESTS", _, !IO)
    else
        ( if NumFailed > 0 then
            io.write_string("SOME TESTS FAILED\n", !IO),
            write_tests_to_file(Failed, "FAILED_TESTS", !IO)
        else
            true
        ),
        ( if NumAborted > 0 then
            io.write_string("SOME TESTS ABORTED\n", !IO),
            write_tests_to_file(Aborted, "ABORTED_TESTS", !IO),
            io.set_exit_status(1, !IO)
        else
            true
        )
    ).


:- pred write_tests_to_file(list(string)::in, string::in,
    io::di, io::uo) is det.

write_tests_to_file(Tests, FileName, !IO) :-
    io.open_output(FileName, OpenResult, !IO),
    (
        OpenResult = ok(File),
        io.write_list(File, Tests, "\n", io.write_string, !IO),
        io.nl(File, !IO)
    ;
        OpenResult = error(IO_Error),
        report_file_open_error(FileName, IO_Error, !IO)
    ).

%-----------------------------------------------------------------------------%

report_file_open_error(FileName, Error, !IO) :-
    io.stderr_stream(Stderr, !IO),
    Msg = io.error_message(Error),
    io.format(Stderr, "error opening file `%s': %s\n",
        [s(FileName), s(Msg)], !IO),
    io.set_exit_status(1, !IO).
