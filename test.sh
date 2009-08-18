rm -rf _local/testapprazor*
if ! cabal build; then
    exit 1
fi
yes | dist/build/apprazor/apprazor testapprazor  &
PID=$!
cd tests
py.test $*
kill $PID
