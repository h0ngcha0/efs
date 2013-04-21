#!/bin/bash

set -e
base_dir=$PWD
env_dir=$base_dir/env
nodes_dir=$base_dir/nodes
riak_dir=$env_dir/riak
riak_repo="git://github.com/basho/riak.git"
efs_be_dir=$nodes_dir/efs_be

case `uname` in
    Linux)   make="make";;
    FreeBSD) make="gmake";;
    Darwin)  make="make";;
    *)       exit 2;;
esac

usage() {
    local script=${0##/*/}
    echo "usage: $script [init | remove_data | start | stop]" 1>&2
    exit 1
}

init() {
    build_env
    build_node
}

build_env() {
    if [ ! -d $riak_dir ] ; then
        cd $env_dir
        echo "cloning riak..."
        clone $riak_dir $riak_repo
        echo "compiling riak..."
        compile_riak $riak_dir
    else
        echo $riak_dir directory already created
    fi
}

build_node() {
    if [ ! -d $nodes_dir ] ; then
        mkdir $nodes_dir

        cd $nodes_dir
        echo "compiling efs backend..."
        compile $efs_be_dir
    else
        echo $nodes_dir directory already created
    fi
}

clone() {
    dir=$1
    giturl=$2

    git clone $giturl $(basename $dir)
}

compile() {
    dir=$1
    cd $dir
    $make
}

compile_riak() {
    dir=$1
    cd $dir
    $make all
    [ -d dev ] || $make devrel
}

start() {
    [ -d $efs_be_dir ] || exit 3

    start_riak

    cd $efs_be_dir
    echo "Starting efs backend..."
    $make devstart
}

start_riak() {
    [ -d $riak_dir ] || exit 3
    echo "starting riak cluster..."

    cd $riak_dir

    for d in dev1 dev2 dev3; do
        ./dev/$d/bin/riak ping > /dev/null 2>&1 || ./dev/$d/bin/riak start
    done

    for d in dev2 dev3; do
        ./dev/$d/bin/riak-admin cluster join dev1@127.0.0.1 || echo "already joined"
    done

    ./dev/$d/bin/riak-admin cluster plan && sleep 3

    ./dev/$d/bin/riak-admin cluster commit
}

stop() {
    stop_riak
}

stop_riak() {
    [ -d $riak_dir ] || exit 3
    echo "Stopping Riak cluster...."
    cd $riak_dir

    for d in dev1 dev2 dev3; do
        ./dev/$d/bin/riak stop || true
    done
}

already_joined() {
    d=$1
    set +e
    ./dev/$d/bin/riak-admin status | grep ring_members | grep -c $d@127.0.0.1 > /dev/null
    set -e
    [ "$?" -eq "0" ]
}

# remove all the data in riak
remove_data() {
    [ -d $riak_dir ] || exit 3
    cd $riak_dir

    echo "WARNING: remove all the riak data..."

    for d in dev1 dev2 dev3; do
        echo "deleting data in ${PWD}/dev/${d}/data..."
        rm -rf ${PWD}/dev/${d}/data
    done
}

# Do the business                                                                                                                                      
[ $# -eq 1 ] || usage
case $1 in
    init)          init;;
    remove_data)   remove_data;;
    start)         start;;
    stop)          stop;;
    *)             usage;;
esac
