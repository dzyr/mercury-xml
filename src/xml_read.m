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

:- module xml_read.
:- interface.


:- import_module assoc_list.
:- import_module io.
:- import_module stream.

%-----------------------------------------------------------------------------%
% Definitions
%-----------------------------------------------------------------------------%
%
% The following terms used in the documentation of this module (e.g. content,
% element, STag) refer to the building pieces of XML defined in EBNF grammar
% notation.
% Source: http://www.w3.org/TR/xml11/
%
%-----------------------------------------------------------------------------%
% content ::= CharData? ((element | Reference | CDSect | PI | Comment)
%                   CharData?)*
%
% CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
%
% element ::= EmptyElemTag | STag content ETag
%
% Reference ::= EntityRef | CharRef
%
% EntityRef ::= '&' Name ';'
%
% Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
%
% EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'
%
% STag ::= '<' Name (S Attribute)* S? '>'
%
% ETag ::= '</' Name S? '>'
%
% Name ::= NameStartChar (NameChar)*
%
% NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] |
%                   [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] |
%                   [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] |
%                   [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] |
%                   [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
%
% NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7 |
%              [#x0300-#x036F] | [#x203F-#x2040]
%
%
% Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] |
%                [#x10000-#x10FFFF]
% any Unicode character, excluding the surrogate blocks, FFFE, and FFFF
%
% Attribute ::= Name Eq AttValue
%
% Eq ::= S? '=' S?
%
% AttValue ::= '"' ([^<&"] | Reference)* '"' |
%              "'" ([^<&'] | Reference)* "'"
%
% S ::= (#x20 | #x9 | #xD | #xA)+
% S (white space) consists of one or more space (#x20) characters,
% carriage returns, line feeds, or tabs
%
%-----------------------------------------------------------------------------%

:- type ok_error(E)
    --->    ok
    ;       error(E).


    % Get XML declaration from the start of the XML stream
    % It may contain version and encoding information
    % XXX return version and encoding instead of ignoring them
    %
    % EBNF: XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
    %
    % Example: <?xml version="1.1" encoding="UTF-8"?>
    %
    %
:- pred get_xml_declaration(io.input_stream::in,
    ok_error(xml_error)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Events that will be recognised in the XML stream
    % An EmptyElemTag triggers two events: STag and ETag. This is the same
    % behaviour as in expat.
    %
:- type content_event
    --->    elem_stag(elem_name, assoc_list(att_name, att_value))
    ;       elem_etag(elem_name)
    ;       data(string).
            % non-whitespace-only textual data
            % leading whitespace is discarded
            % trailing whitespace is included
            % XXX decode EntityRef (&amp; &apos; &lt; &gt; &quot) to
            % their corresponding characters (& ' < > ")


:- type elem_name
    --->    elem_name(string).


:- type att_name
    --->    att_name(string).


:- type att_value
    --->    att_value(string).

%-----------------------------------------------------------------------------%

:- type xml_error
    --->    unexpected_eof
    ;       unknown_entity_ref_name
    %;       malformed_comment_start
    %;       malformed_comment
    ;       invalid_name_start_char
    ;       invalid_char_in_att_value
    ;       unexpected_start_of_xml_decl
    ;       expecting_gt_after_slash
    ;       expecting_gt_at_end_of_xml_decl
    ;       expecting_eq_past_att_name
    ;       expecting_att_value_start_quote_past_eq
    ;       expecting_attribute_or_end_of_tag
    ;       expecting_name_start_char_or_slash_after_lt
    ;       expecting_gt_after_etag_name
    ;       io_error(io.error).

%-----------------------------------------------------------------------------%

    % Applies the given closure to each content_event read from the
    % input stream until eof or error.
    % Usage: fold_content(EventAccPred, Stream, Acc0, Result, !IO)
    %
    % Unlike the C streaming parser expat, you don't register event handler
    % functions. Every STag and ETag will be returned. Textual data will only
    % trigger an event if it is not whitespace-only. This behaviour is choosen
    % to suppress whitespace that is only used to format/indent the XML.
    %
    % If textual data is not whitepace-only, it will be passed to the
    % accumulator predicate, ignoring leading whitespace.
    %
:- pred fold_content(
    pred(content_event, T, T)::in(pred(in, in, out) is det),
    io.input_stream::in, T::in, stream.maybe_partial_res(T, xml_error)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%
% XXX evaluate possible speedup of these changes
% - Parallelisation of folding over content: continue reading the stream while
%   accumulating the just read content in parallel
% - Use pragma inline
% - Classify character tokens with a lookup table instead of a sequence of
%   if-then-else, similar to lexer.lookup_token_action
% - Read stream into buffer and parse this buffer instead of reading single
%   chars from the stream
%-----------------------------------------------------------------------------%

get_xml_declaration(Stream, X, !IO) :-
    read_keyword_chars(Stream, ['<', '?', 'x', 'm', 'l'],
        unexpected_start_of_xml_decl, StartResult, !IO),
    (
        StartResult = ok,
        get_xml_declaration_until_end(Stream, EndResult, !IO),
        (
            EndResult = ok,
            X = ok
        ;
            EndResult = error(EndError),
            X = error(EndError)
        )
    ;
        StartResult = error(StartError),
        X = error(StartError)
    ).


:- pred get_xml_declaration_until_end(io.input_stream::in,
    ok_error(xml_error)::out, io::di, io::uo) is det.

get_xml_declaration_until_end(Stream, X, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = ok,
        ( if Char = ('?') then
            io.read_char(Stream, NextResult, !IO),
            (
                NextResult = ok(NextChar),
                ( if NextChar = ('>') then
                    X = ok
                else
                    X = error(expecting_gt_at_end_of_xml_decl)
                )
            ;
                NextResult = eof,
                X = error(unexpected_eof)
            ;
                NextResult = error(NextErrorMsg),
                X = error(io_error(NextErrorMsg))
            )
        else
            get_xml_declaration_until_end(Stream, X, !IO)
        )
    ;
        Result = eof,
        X = error(unexpected_eof)
    ;
        Result = error(Msg),
        X = error(io_error(Msg))
    ).

%-----------------------------------------------------------------------------%

fold_content(Pred, Stream, Acc0, X, !IO) :-
    get_content_event(Stream, Result, !IO),
    (
        Result = ok(EventOrEof),
        (
            EventOrEof = event(current_and_next_event(Event, MaybeNextEvent)),
            Pred(Event, Acc0, Acc1),
            (
                MaybeNextEvent = next_event_unknown_yet,
                Acc = Acc1
            ;
                MaybeNextEvent = etag_past_empty_elem_tag(ElemName),
                Pred(elem_etag(ElemName), Acc1, Acc)
            ),
            fold_content(Pred, Stream, Acc, X, !IO)
        ;
            EventOrEof = eof_event,
            X = ok(Acc0)
        )
    ;
        Result = error(Error),
        X = error(Acc0, Error)
    ).

%-----------------------------------------------------------------------------%

:- type current_and_next_event
    --->    current_and_next_event(content_event, maybe_next_event).


:- type event_or_eof
    --->    event(current_and_next_event)
    ;       eof_event.


    % An EmptyElemTag will trigger two separate events: STag and ETag
    %
:- type maybe_next_event
    --->    etag_past_empty_elem_tag(elem_name)
    ;       next_event_unknown_yet.


    % Recogise STag, ETag, EmptyElemTag and textual data
    % Whitespace between markup will be ignored
    % Non-whitespace textual data will be recognised (discarding leading
    % whitespace)
    %
    % XXX ignore Comment
    %
:- pred get_content_event(io.input_stream::in,
    maybe_error(event_or_eof, xml_error)::out,
    io::di, io::uo) is det.

get_content_event(Stream, X, !IO) :-
    get_content_token(Stream, Result, !IO),
    (
        Result = ok(Token),
        (
            Token = stag_or_empty_elem_tag_start(NameStartChar),
            get_stag_or_empty_elem_tag(Stream, NameStartChar, TagResult, !IO),
            (
                TagResult = ok(Event),
                X = ok(event(Event))
            ;
                TagResult = error(TagError),
                X = error(TagError)
            )
        ;
            Token = etag_start,
            get_etag(Stream, EtagElemNameResult, !IO),
            (
                EtagElemNameResult = ok(EtagElemName),
                X = ok(event(current_and_next_event(
                    elem_etag(EtagElemName),
                    next_event_unknown_yet
                )))
            ;
                EtagElemNameResult = error(EtagElemNameError),
                X = error(EtagElemNameError)
            )
        ;
            Token = non_whitespace_char(TextStartChar),
            get_textual_data(Stream, [TextStartChar], TextResult, !IO),
            (
                TextResult = ok(TextString),
                X = ok(event(current_and_next_event(
                    data(TextString),
                    next_event_unknown_yet
                )))
            ;
                TextResult = error(TextError),
                X = error(TextError)
            )
        ;
            Token = eof,
            X = ok(eof_event)
        )
    ;
        Result = error(Msg),
        X = error(Msg)
    ).

%-----------------------------------------------------------------------------%

:- pred get_stag_or_empty_elem_tag(io.input_stream::in, char::in,
    maybe_error(current_and_next_event, xml_error)::out,
    io::di, io::uo) is det.

get_stag_or_empty_elem_tag(Stream, NameStartChar, X, !IO) :-
    get_rest_of_name(Stream, NameStartChar, ElemNameResult, !IO),
    (
        ElemNameResult = ok(ElemNameString),
        acc_attributes(Stream, [], AttResults, !IO),
        (
            AttResults = ok(attributes_and_tag_type(Attributes, TagType)),
            ElemName = elem_name(ElemNameString),
            (
                TagType = stag,
                X = ok(current_and_next_event(
                    elem_stag(ElemName, Attributes),
                    next_event_unknown_yet
                ))
            ;
                TagType = empty_elem_tag,
                X = ok(current_and_next_event(
                    elem_stag(ElemName, Attributes),
                    etag_past_empty_elem_tag(ElemName)
                ))
            )
        ;
            AttResults = error(AttError),
            X = error(AttError)
        )
    ;
        ElemNameResult = error(ElemNameError),
        X = error(ElemNameError)
    ).

%-----------------------------------------------------------------------------%

    % Get rest of ETag after initial "</" token
    %
:- pred get_etag(io.input_stream::in, maybe_error(elem_name, xml_error)::out,
    io::di, io::uo) is det.

get_etag(Stream, X, !IO) :-
    get_name(Stream, ElemNameResult, !IO),
    (
        ElemNameResult = ok(ElemName),
        get_optional_whitespace_until_gt(Stream, EtagCloseResult, !IO),
        (
            EtagCloseResult = ok,
            X = ok(elem_name(ElemName))
        ;
            EtagCloseResult = error(EtagCloseError),
            X = error(EtagCloseError)
        )
    ;
        ElemNameResult = error(ElemNameError),
        X = error(ElemNameError)
    ).


:- pred get_optional_whitespace_until_gt(io.input_stream::in,
    ok_error(xml_error)::out, io::di, io::uo) is det.

get_optional_whitespace_until_gt(Stream, X, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = ok,
        ( if is_xml_whitespace(Char) then
            get_optional_whitespace_until_gt(Stream, X, !IO)
        else if Char = ('>') then
            X = ok
        else
            X = error(expecting_gt_after_etag_name)
        )
    ;
        Result = eof,
        X = error(unexpected_eof)
    ;
        Result = error(Msg),
        X = error(io_error(Msg))
    ).

%-----------------------------------------------------------------------------%

:- type content_token
    --->    stag_or_empty_elem_tag_start(char)  % '<' including NameStartChar
    ;       etag_start                          % "</"
    ;       non_whitespace_char(char)           % ignore leading whitespace
    ;       eof.


    % Get next content token while ignoring leading whitespace
    %
:- pred get_content_token(io.input_stream::in,
    maybe_error(content_token, xml_error)::out,
    io::di, io::uo) is det.

get_content_token(Stream, X, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = ok,
        ( if is_xml_whitespace(Char) then
            get_content_token(Stream, X, !IO)
        else if Char = ('<') then
            get_content_token_past_left_angle_bracket(Stream, X, !IO)
        else
            X = ok(non_whitespace_char(Char))
        )
    ;
        Result = eof,
        X = ok(eof)
    ;
        Result = error(Msg),
        X = error(io_error(Msg))
    ).


:- pred get_content_token_past_left_angle_bracket(io.input_stream::in,
    maybe_error(content_token, xml_error)::out,
    io::di, io::uo) is det.

get_content_token_past_left_angle_bracket(Stream, X, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = ok,
        ( if is_name_start_char(Char) then
            X = ok(stag_or_empty_elem_tag_start(Char))
        else if Char = ('/') then
            X = ok(etag_start)
        else
            X = error(expecting_name_start_char_or_slash_after_lt)
        )
    ;
        Result = eof,
        X = error(unexpected_eof)
    ;
        Result = error(Msg),
        X = error(io_error(Msg))
    ).

%-----------------------------------------------------------------------------%

    % Get textual data until next content_token
    % XXX decode EntityRef (&amp; &apos; &lt; &gt; &quot) to their
    %   corresponding characters (& ' < > ")
    %
:- pred get_textual_data(io.input_stream::in, list(char)::in,
    maybe_error(string, xml_error)::out,
    io::di, io::uo) is det.

get_textual_data(Stream, Acc0, X, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = ok,
        ( if Char = ('<') then
            io.putback_char(Stream, Char, !IO),
            X = ok(string.from_rev_char_list(Acc0))
        else
            get_textual_data(Stream, [Char | Acc0], X, !IO)
        )
    ;
        Result = eof,
        X = error(unexpected_eof)
    ;
        Result = error(Msg),
        X = error(io_error(Msg))
    ).


%-----------------------------------------------------------------------------%

:- type scanned_past_whitespace
    --->    scanned_past_whitespace
    ;       not_scanned_past_whitespace.


:- type token_past_elem_name
    --->    closing_gt
    ;       closing_slash_gt
    ;       name_start_char(char).


    % Get next token past Name of a STag or EmptyElemTag
    % There might be these possibilities
    % - Optional whitespace followed by '>'
    % - Optional whitespace followed by "/>"
    % - Whitespace followed by NameStartChar
    %
:- pred get_token_past_elem_name(io.input_stream::in,
    scanned_past_whitespace::in,
    maybe_error(token_past_elem_name, xml_error)::out,
    io::di, io::uo) is det.

get_token_past_elem_name(Stream, PastWhitespace, X, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = ok,
        ( if Char = ('>') then
            X = ok(closing_gt)
        else if Char = ('/') then
            read_keyword_chars(Stream, ['>'], expecting_gt_after_slash,
                ResultAfterSlash, !IO),
            (
                ResultAfterSlash = ok,
                X = ok(closing_slash_gt)
            ;
                ResultAfterSlash = error(ErrorAfterSlash),
                X = error(ErrorAfterSlash)
            )
        else if is_xml_whitespace(Char) then
            get_token_past_elem_name(Stream, scanned_past_whitespace, X, !IO)
        else if is_name_start_char(Char) then
            (
                PastWhitespace = scanned_past_whitespace,
                X = ok(name_start_char(Char))
            ;
                PastWhitespace = not_scanned_past_whitespace,
                X = error(expecting_attribute_or_end_of_tag)
            )
        else
            X = error(expecting_attribute_or_end_of_tag)
        )
    ;
        Result = eof,
        X = error(unexpected_eof)
    ;
        Result = error(Msg),
        X = error(io_error(Msg))
    ).

%-----------------------------------------------------------------------------%

    % Get Name
    % EBNF: Name ::= NameStartChar (NameChar)*
    %
:- pred get_name(io.input_stream::in,
    maybe_error(string, xml_error)::out, io::di, io::uo) is det.

get_name(Stream, X, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = ok,
        ( if is_name_start_char(Char) then
            get_rest_of_name(Stream, Char, X, !IO)
        else
            X = error(invalid_name_start_char)
        )
    ;
        Result = eof,
        X = error(unexpected_eof)
    ;
        Result = error(Msg),
        X = error(io_error(Msg))
    ).


    % Get rest of Name after NameStartChar
    %
:- pred get_rest_of_name(io.input_stream::in, char::in,
    maybe_error(string, xml_error)::out, io::di, io::uo) is det.

get_rest_of_name(Stream, NameStartChar, X, !IO) :-
    get_rest_of_name2(Stream, [NameStartChar], Result, !IO),
    (
        Result = ok(Chars),
        X = ok(string.from_rev_char_list(Chars))
    ;
        Result = error(Error),
        X = error(Error)
    ).


    % Get rest of Name after NameStartChar
    %
:- pred get_rest_of_name2(io.input_stream::in, list(char)::in,
    maybe_error(list(char), xml_error)::out, io::di, io::uo) is det.

get_rest_of_name2(Stream, Acc0, X, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = ok,
        ( if is_name_char(Char) then
            get_rest_of_name2(Stream, [Char | Acc0], X, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            X = ok(Acc0)
        )
    ;
        Result = eof,
        X = error(unexpected_eof)
    ;
        Result = error(Msg),
        X = error(io_error(Msg))
    ).

%-----------------------------------------------------------------------------%

:- type attributes_and_tag_type
    --->    attributes_and_tag_type(
                attributes  :: assoc_list(att_name, att_value),
                tag_type    :: stag_or_empty_elem_tag
            ).


:- type stag_or_empty_elem_tag
    --->    stag
    ;       empty_elem_tag.


    % Accumulate attributes until closing '>' or "/>"
    %
:- pred acc_attributes(io.input_stream::in,
    assoc_list(att_name, att_value)::in,
    maybe_error(attributes_and_tag_type, xml_error)::out,
    io::di, io::uo) is det.

acc_attributes(Stream, Acc0, X, !IO) :-
    get_token_past_elem_name(Stream, not_scanned_past_whitespace, Result,
        !IO),
    (
        Result = ok(Token),
        (
            Token = closing_gt,
            X = ok(attributes_and_tag_type(Acc0, stag))
        ;
            Token = closing_slash_gt,
            X = ok(attributes_and_tag_type(Acc0, empty_elem_tag))
        ;
            Token = name_start_char(Char),
            get_rest_of_attribute(Stream, Char, AttResult, !IO),
            (
                AttResult = ok(Att),
                acc_attributes(Stream, [Att | Acc0], X, !IO)
            ;
                AttResult = error(AttError),
                X = error(AttError)
            )
        )
    ;
        Result = error(Msg),
        X = error(Msg)
    ).

%-----------------------------------------------------------------------------%

    % Get rest of attribute after NameStartChar
    %
:- pred get_rest_of_attribute(io.input_stream::in, char::in,
    maybe_error(pair(att_name, att_value), xml_error)::out,
    io::di, io::uo) is det.

get_rest_of_attribute(Stream, NameStartChar, X, !IO) :-
    get_rest_of_name(Stream, NameStartChar, GetNameResult, !IO),
    (
        GetNameResult = ok(AttName),
        get_eq_until_att_value_start_quote(Stream, EqQuoteResult, !IO),
        (
            EqQuoteResult = ok(StartQuote),
            get_attribute_value(Stream, StartQuote, [], AttValueResult, !IO),
            (
                AttValueResult = ok(Chars),
                X = ok(
                    att_name(AttName) -
                    att_value(string.from_rev_char_list(Chars))
                )
            ;
                AttValueResult = error(AttValueError),
                X = error(AttValueError)
            )
        ;
            EqQuoteResult = error(EqQuoteError),
            X = error(EqQuoteError)
        )
    ;
        GetNameResult = error(RestOfNameError),
        X = error(RestOfNameError)
    ).

%-----------------------------------------------------------------------------%

    % Get Eq until single or double quote indicating the AttValue start char
    % EBNF: Eq ::= S? '=' S?
    %
:- pred get_eq_until_att_value_start_quote(io.input_stream::in,
    maybe_error(att_value_quotation, xml_error)::out, io::di, io::uo) is det.

get_eq_until_att_value_start_quote(Stream, X, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = ok,
        ( if is_xml_whitespace(Char) then
            get_eq_until_att_value_start_quote(Stream, X, !IO)
        else if Char = ('=') then
            get_att_value_start_quote(Stream, StartQuoteResult, !IO),
            (
                StartQuoteResult = ok(StartQuote),
                X = ok(StartQuote)
            ;
                StartQuoteResult = error(StartQuoteError),
                X = error(StartQuoteError)
            )
        else
            X = error(expecting_eq_past_att_name)
        )
    ;
        Result = eof,
        X = error(unexpected_eof)
    ;
        Result = error(Error),
        X = error(io_error(Error))
    ).


:- pred get_att_value_start_quote(io.input_stream::in,
    maybe_error(att_value_quotation, xml_error)::out, io::di, io::uo) is det.

get_att_value_start_quote(Stream, X, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = ok,
        ( if is_xml_whitespace(Char) then
            get_att_value_start_quote(Stream, X, !IO)
        else if is_att_value_start_char(Char, AttValueQuote) then
            X = ok(AttValueQuote)
        else
            X = error(expecting_att_value_start_quote_past_eq)
        )
    ;
        Result = eof,
        X = error(unexpected_eof)
    ;
        Result = error(Error),
        X = error(io_error(Error))
    ).

%-----------------------------------------------------------------------------%

:- type att_value_quotation
    --->    double_quote
    ;       single_quote.


:- pred is_att_value_start_char(char::in, att_value_quotation::out) is semidet.

is_att_value_start_char('"',    double_quote).
is_att_value_start_char('\'',   single_quote).

%-----------------------------------------------------------------------------%

    % Get Attribute value until closing double quote
    % EntityRef will be decoded
    %
:- pred get_attribute_value(io.input_stream::in, att_value_quotation::in,
    list(char)::in, maybe_error(list(char), xml_error)::out,
    io::di, io::uo) is det.

get_attribute_value(Stream, StartQuote, Acc0, X, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = ok,
        ( if is_special_att_value_token(Char, StartQuote, Token) then
            (
                Token = closing_quote,
                X = ok(Acc0)
            ;
                Token = ampersand,
                % XXX implement CharRef
                %   The ampersand indicates the start of a Reference, which
                %   is an EntityRef or a CharRef.
                %
                get_entity_ref(Stream, EntityRefResult, !IO),
                (
                    EntityRefResult = ok(EntityRef),
                    get_attribute_value(Stream, StartQuote,
                        [decode_entity_ref(EntityRef) | Acc0], X, !IO)
                ;
                    EntityRefResult = error(EntityRefError),
                    X = error(EntityRefError)
                )
            ;
                Token = left_angle_bracket,
                X = error(invalid_char_in_att_value)
            )
        else
            get_attribute_value(Stream, StartQuote, [Char | Acc0], X, !IO)
        )
    ;
        Result = eof,
        X = error(unexpected_eof)
    ;
        Result = error(Msg),
        X = error(io_error(Msg))
    ).

%-----------------------------------------------------------------------------%
% Classify character tokens
%-----------------------------------------------------------------------------%

:- type att_value_token
    --->    ampersand
    ;       closing_quote
    ;       left_angle_bracket.


:- pred is_special_att_value_token(char::in, att_value_quotation::in,
    att_value_token::out) is semidet.

is_special_att_value_token('&',     _,              ampersand).
is_special_att_value_token('"',     double_quote,   closing_quote).
is_special_att_value_token('\'',    single_quote,   closing_quote).
is_special_att_value_token('<',     _,              left_angle_bracket).


    % True iff the character is a whitespace character according to xml
    %
:- pred is_xml_whitespace(char::in) is semidet.

is_xml_whitespace(' ').     % '\x20\' space
is_xml_whitespace('\t').    % '\x09\' horizontal tab
is_xml_whitespace('\r').    % '\x0D\' carriage return
is_xml_whitespace('\n').    % '\x0A\' line feed


    % XXX evaluate speedup by use of a lookup table similar to
    %   lexer.lookup_token_action instead of a sequence of if-then-else
    %
:- pred is_name_start_char(char::in) is semidet.

is_name_start_char(Char) :-
    ( if Char = (':') then
        true
    else if char.is_alpha_or_underscore(Char) then
        true
    else if
        Char @>= ('\xC0\'),
        Char @=< ('\xD6\')
        % Latin-1 Supplement (Unicode block) letters
    then
        true
    else if
        Char @>= ('\xD8\'),
        Char @=< ('\xF6\')
        % Latin-1 Supplement (Unicode block) letters
    then
        true
    else
        fail
    ).


    % XXX evaluate speedup by use of a lookup table similar to
    %   lexer.lookup_token_action instead of a sequence of if-then-else
    %
:- pred is_name_char(char::in) is semidet.

is_name_char(Char) :-
    ( if is_name_start_char(Char) then
        true
    else if Char = ('-') then
        true
    else if Char = ('.') then
        true
    else if is_digit(Char) then
        true
    else if Char = ('\xB7\') then
        true
    else
        fail
    ).

%-----------------------------------------------------------------------------%
% Recognise EntityRef in character input stream
%-----------------------------------------------------------------------------%

:- type entity_ref
    --->    amp
    ;       lt
    ;       gt
    ;       apos
    ;       quot.


:- func decode_entity_ref(entity_ref) = char.

decode_entity_ref(amp) = ('&').
decode_entity_ref(lt) = ('<').
decode_entity_ref(gt) = ('>').
decode_entity_ref(apos) = ('\'').
decode_entity_ref(quot) = ('"').


    % Recognise the following EntityRef names in the input stream after the
    % initial ampersand character: amp | apos | gt | lt | quot
    % XXX simplify code by using trie data type for names?
    %
:- pred get_entity_ref(io.input_stream::in,
    maybe_error(entity_ref, xml_error)::out, io::di, io::uo) is det.

get_entity_ref(Stream, Result, !IO) :-
    io.read_char_unboxed(Stream, ReadCharResult, Char, !IO),
    (
        ReadCharResult = error(Error),
        Result = error(io_error(Error))
    ;
        ReadCharResult = eof,
        Result = error(unexpected_eof)
    ;
        ReadCharResult = ok,
        ( if Char = 'a' then
            read_amp_apos(Stream, Result, !IO)
        else if Char = 'q' then
            read_keyword_chars(Stream, ['u', 'o', 't', ';'],
                unknown_entity_ref_name, X, !IO),
            (
                X = ok,
                Result = ok(quot)
            ;
                X = error(Err),
                Result = error(Err)
            )
        else if Char = 'l' then
            read_keyword_chars(Stream, ['t', ';'],
                unknown_entity_ref_name, X, !IO),
            (
                X = ok,
                Result = ok(lt)
            ;
                X = error(Err),
                Result = error(Err)
            )
        else if Char = 'g' then
            read_keyword_chars(Stream, ['t', ';'],
                unknown_entity_ref_name, X, !IO),
            (
                X = ok,
                Result = ok(gt)
            ;
                X = error(Err),
                Result = error(Err)
            )
        else Result = error(unknown_entity_ref_name)
        )
    ).


:- pred read_amp_apos(io.input_stream::in,
    maybe_error(entity_ref, xml_error)::out, io::di, io::uo) is det.

read_amp_apos(Stream, Result, !IO) :-
    io.read_char_unboxed(Stream, ReadCharResult, Char, !IO),
    (
        ReadCharResult = error(Error),
        Result = error(io_error(Error))
    ;
        ReadCharResult = eof,
        Result = error(unexpected_eof)
    ;
        ReadCharResult = ok,
        ( if Char = 'm' then
            read_keyword_chars(Stream, ['p', ';'], unknown_entity_ref_name,
                X, !IO),
            (
                X = ok,
                Result = ok(amp)
            ;
                X = error(Err),
                Result = error(Err)
            )
        else if Char = 'p' then
            read_keyword_chars(Stream, ['o', 's', ';'],
                unknown_entity_ref_name, X, !IO),
            (
                X = ok,
                Result = ok(apos)
            ;
                X = error(Err),
                Result = error(Err)
            )
        else Result = error(unknown_entity_ref_name)
        )
    ).

%-----------------------------------------------------------------------------%

    % Check input stream against list of chars.
    %
:- pred read_keyword_chars(io.input_stream::in, list(char)::in, xml_error::in,
    ok_error(xml_error)::out, io::di, io::uo) is det.

read_keyword_chars(_, [], _, ok, !IO).

read_keyword_chars(Stream, [H|T], XmlError, X, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = ok,
        ( if Char = H then
            read_keyword_chars(Stream, T, XmlError, X, !IO)
        else
            X = error(XmlError)
        )
    ;
        Result = eof,
        X = error(unexpected_eof)
    ;
        Result = error(Error),
        X = error(io_error(Error))
    ).
