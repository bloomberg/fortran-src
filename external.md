## External Representation


``` 
span ::= "(<start-line>:<start-col>)-(<end-line>:<end-col>)"
```


```
program_unit
  ::= { "tag": "main", "name": string, "blocks": [block] }
   |  { "tag": "subroutine", "name": string, "recursive": bool, "arguments": [expression]?, "blocks": [block] }
   |  { "tag": "function", "name": string, "recursive": bool, "arguments": [expression]?, "type": type_spec, "blocks": [block], "result": expression? }
```

```
block
  ::= { "tag": "statement", "label": label?, "statement": statement }
   |  { "tag": "if", "label": label?, "conditions": [expression?], "blocks": [[block]], "end_label": label? }
   |  { "tag": "select", "label": label?, "scrutinee": expression, "ranges": [index?], "blocks": [[block]], "end_label": label? }
   |  { "tag": "do", "label": label?, "target": label?, "do_spec": do_specification, "body": [block], "end_label": label? }
   |  { "tag": "do_while", "label": label?, "target": label?, "condition": expression, "body": [block], "end_label": label? }
```

```
statement
  ::= { "tag": "declaration", "type": type_spec, "attributes": [attribute]?, "declarators": [declarator] }
   |  { "tag": "structure", "name": string, "fields": [structure_item] }
   |  { "tag": "save", "variables": [expression]? }
   |  { "tag": "dimension", "declarators": [declarator] }
   |  { "tag": "pointer", "declarators": [declarator] }
   |  { "tag": "data", "data_groups": [data_group] }
   |  { "tag": "automatic", "declarators": [declarator] }
   |  { "tag": "parameter", "declarators": [declarator] }
   |  { "tag": "external", "arguments": [expression] }
   |  { "tag": "intrinsic", "arguments": [expression] }
   |  { "tag": "common", "common_groups": [common_group] }
   |  { "tag": "equivalence", "groups": [[expression]] }
   |  { "tag": "implicit", "implicit_items": [implicit_items]? }
   |  { "tag": "entry", "name": expression, "arguments": [expression]? }
   |  { "tag": "include", "path": expression, "blocks": [block]? }
   <!-- |  { tag: "do" } -->
   <!-- |  { tag: "do_while" } -->
   |  { "tag": "end_do" }
   |  { "tag": "cycle" }
   |  { "tag": "exit" }
   |  { "tag": "if_logical", "condition": expression, "statement": statement }
   |  { "tag": "if_arithmetic", "expression": expression, "less": expression, "equal": expression, "greater": expression }
   |  { "tag": "function", "name": expression, "arguments": [expression], "body": expression }
   |  { "tag": "assign_expression", "target": expression, "expression": expression }
   |  { "tag": "assign_label", "target": expression, "label": expression }
   |  { "tag": "goto", "target": expression }
   |  { "tag": "goto_assigned", "target": expression, "labels": [expression]? }
   |  { "tag": "goto_computed", "target": expression, "labels": [expression] }
   |  { "tag": "call", "function": expression, "arguments": [argument]? }
   |  { "tag": "return", "target": expression? }
   |  { "tag": "continue" }
   |  { "tag": "stop", "message": expression? }
   |  { "tag": "pause", "message": expression? }
   |  { "tag": "read", "format": ([control_pair] | expression), "arguments": [expression]? }
   |  { "tag": "write", "format": [control_pair], "arguments": [expression]? }
   |  { "tag": "print", "format": expression, "arguments": [expression]? }
   |  { "tag": "type", "format": expression, "arguments": [expression]? }
   |  { "tag": "open", "specification": [control_pair] }
   |  { "tag": "close", "specification": [control_pair] }
   |  { "tag": "inquire", "specification": [control_pair] }
   |  { "tag": "rewind", "specification": ([control_pair] | expression) }
   |  { "tag": "backspace", "specification": ([control_pair] | expression) }
   |  { "tag": "endfile", "specification": ([control_pair] | expression) }
```
