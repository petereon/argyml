# Introduction

`argyml` is a command line tool that converts CLI arguments, options and flags into a YAML for downstream parsing

## Motivation

While there are spectacularly powerful CLI tools and shell-scripts, CLI parsing in shell itself remains a manual and error prone process.
`argyml` aims to solve this by operating in tandem with [`yq`](https://github.com/mikefarah/yq) to allow for more structured CLI argument parsing.

### Example

```zsh
get_host () {
    args=$(argyml $@)
    host=$(echo $args | yq eval '.options.[] | select(.key == "*host*") | .value' )
    echo $host
}
```

```sh
$ get_host --host 127.0.0.1
127.0.0.1
```

## Usage

Usage of `argyml` is really easy, you pass it command-line arguments, options and flags:

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

`argyml` parses the arguments on naive basis. Implications are the following:

- it cannot be provided with any sort of *schema* for options, flags and arguments or their types [^1]

- `arguments` cannot follow `flags`, otherwise they will be interpreted as `options`

## Roadmap

- [x] Refactor
- [ ] Option to directly generate a bash associative array?

[^1]: For type casting you should rely on downstream.
