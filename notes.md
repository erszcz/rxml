# 2016-11-04 exml\_stream:parse\_events/4 problem

See:

- parse-events.exml.log:173, parse-events.rxml.log:173
- parse-events.exml.log:464, parse-events.rxml.log:414

What we see there is that the exml events are:

```
{xml_element_start, <<"stream:features">>, ...}
...
{xml_element_end, <<"stream:features">>}
```

but the rxml events are:

```
{xml_element_start, <<"stream:features">>, ...}
...
{xml_element_end, <<"features">>}
```

therefore `parse_events/4` can't properly match the opening and closing tags
from the stack and return the `<stream:features/>` element when parsed.
Can we get this information from the RustyXML parser and return along
with the `xml_element_end` event?
