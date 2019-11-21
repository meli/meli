set language rust
source ~/.gdbinit
break rust_panic
set auto-load python-scripts
set print thread-events off
