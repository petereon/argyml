get_host () {
    args=$(argyml $@)
    host=$(echo $args | yq eval '.options.[] | select(.key == "*host*") | .value' )
    echo $host
}