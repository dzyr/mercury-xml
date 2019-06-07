# Mercury-XML
Mercury-XML provides a [Mercury](http://www.mercurylang.org) module for event-based parsing of XML streams.

The idea of parsing XML streams as events with Mercury is inspired by [expat](https://en.wikipedia.org/wiki/Expat_(library)) and [SAX](https://en.wikipedia.org/wiki/Simple_API_for_XML).

Mercury-XML handles these events: start tags, end tags and textual data.


## Usage
Use predicate `xml_read.fold_content/6` to fold over the events of an XML stream and accumulate them as arbitrary data structure.

To accumulate XML events in a list:

    xml_read.fold_content(list.cons, Stream, [], Result, !IO)

Unlike expat and SAX, you don't register event handler functions. Every STag and ETag will be passed to the accumulator predicate. Textual data will only trigger an event if it is not whitespace-only. This behaviour is choosen to suppress whitespace that is only used to format/indent the XML.

If textual data is not whitepace-only, it will be passed to the accumulator predicate, ignoring leading whitespace.

You might accumulate XML events in a custom data structure directly or build a generic XML-DOM first and query it later.


## Example
A valid XML document can be parsed with this this code:

```Mercury
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
```

When applied to an input stream from this sample XML document:

```xml
<?xml version="1.1" encoding="UTF-8"?>
<doc>
	<Element Attribute="Value">Text</Element>
</doc>
```

it will produce this result:
```Mercury
ok([
    elem_stag(elem_name("doc"), []),
    elem_stag(elem_name("Element"), [
        att_name("Attribute") - att_value("Value")
    ]),
    data("Text"),
    elem_etag(elem_name("Element")),
    elem_etag(elem_name("doc"))
])
```

Parsing the XML stream in the example consists of these steps:
1. Read the XML declaration with `get_xml_declaration/4`.
2. Fold over the XML events of the stream with `fold_content/6` and use `list.cons` to accumulate XML events in a list.

This example is part of the test cases. You can find it here to see it in action:
- The predicate in `tests/test_xml.m`
- The XML input file in `tests/example.inp` and
- The expected result in `tests/test_cases.m`.


## LICENSE
Code is licensed under [Mozilla Public License 2.0](LICENSE-MPL-2.0.txt).

Trivial bits of code and test cases (not the testing framework) are dedicated to the public domain by using an explicit Public Domain dedication in the header of the respective files.


## Installation
* Add mercury-xml as a git submodule to your project:
  `git submodule add https://path-to/mercury-xml.git`
* Choose between two methods to access the xml module
  * The Mercury.modules method
  * The library method


### The Mercury.modules method
    mmc -f ../mercury-xml/src/xml_read.m
Example: have a look at Makefile-target Mercury.modules in tests/Makefile

Add this to your Makefile to update the submodule by typing a simple:
    make init

```make
.PHONY: init
init:
	git submodule update --init

```

### The library method
Add this to your Makefile and type `make init`:

```make
.PHONY: init
init:
	git submodule update --init
	$(MAKE) install-mercury-xml

.PHONY: install-mercury-xml
install-mercury-xml:
	cd mercury-xml && $(MAKE) default
	cd mercury-xml && $(MAKE) install
```

The library will be installed in the mercury-xml/.sandbox folder by default. This allows you to install different versions of the library across different projects.

Don't forget to tell the Mercury compiler the place where the library is installed by writing this into your src/Mercury.options file:
```
MCFLAGS = --mld ../mercury-xml/.sandbox/lib/mercury

```

## Testing
Run the regression test suite with:

    $ make test


## Benchmarking
Time complexity of parsing an XML stream should be linear depending on the size of the XML file if you use a constant time accumulator predicate like list.cons.

Start benchmarking by:

    $ cd tests
    $ make benchmark

This writes a large XML test file (100 MB) to your hard disk and measures the time to parse it.


## ToDo
* Ignore comments in XML documents
* Add other options to parse textual data
* Handle encodings other than UTF-8
* Various other ToDo's and ideas: search for XXX in the source code


## Author
Dirk Ziegemeyer <dirk@ziegemeyer.de>
