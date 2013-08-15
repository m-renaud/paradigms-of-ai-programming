#!/usr/bin/env python3

# Provide debugging facilities that can easily be turned on and
# off through a debug symbol list.

debug_symbol_list = []

def write(symbol, msg):
    """If SYMBOL is in the debug list, print message."""
    if symbol in debug_symbol_list:
        print(msg)

def enable(symbol):
    """Enable debugging for SYMBOL."""
    if symbol not in debug_symbol_list:
        debug_symbol_list.append(symbol)
    return debug_symbol_list

def disable(symbol):
    """Disable debugging for SYMBOL."""
    if symbol in debug_symbol_list:
        debug_symbol_list.remove(symbol)
