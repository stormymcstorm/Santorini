# santorini

## Building
```
stack build --copy-bins
```
The `--copy-bins` flag will copy the `santorini-exe` binary to `~/.local/bin`

## Running
Extract the referee program
```
tar xvzf santorini-linux.tgz
```
Run
```
santorini-linux/bin/run ~/.local/bin/santorini-exe santorini-linux/bin/play-search
```