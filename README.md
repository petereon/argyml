# Introduction

**argyml** is a command line tool that converts CLI arguments, options and flags into a YAML for downstream parsing

## Usage

Usage of **argyml** is really easy, you pass it command-line flags:

```sh
argyml some arguments --host 127.0.0.1 -i --another-flag
```
and receive YAML in standard output:

```yaml
options:
  - key: "--host"
    value: "127.0.0.1"
flags:
  - "-i"
  - "--another-flag"
arguments:
  - "some"
  - "arguments"
```

## Limitations

**argyml** parses the arguments on naive basis. Implications are the following:

- it cannot be provided with any sort of *schema* for options, flags and arguments or their types [^1]

- `arguments` cannot follow `flags`, otherwise they will be interpreted as `options`

[^1]: For type casting you should rely on downstream.
