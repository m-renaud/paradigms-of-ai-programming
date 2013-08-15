#!/usr/bin/env python3

# This code is an adaption of Peter Norvig's code from PAIP.

import debug

def append(x, y):
    """Append the list y to the list x."""
    x.extend(y)
    return x

def prepend(x, y):
    """Prepend the list y to the list x."""
    y.extend(x)
    return y
    
def binary_tree(x):
    """Given a node in a binary tree, generate a list of it's successors."""
    return [2*x, 2*x + 1]


def eql(value):
    """Create a function to test if a value matches."""
    return lambda x: x == value


def tree_search(states, goal_test, successors, combiner):
    """Find a state in STATES that satisfies GOAL_TEST. Start with
    STATES and search according to the SUCCESSOR and COMBINER functions"""
    debug.write("tree-search", "Searching: " + str(states).strip('[]'))

    if len(states) == 0:
        return None
    elif goal_test(states[0]):
        return states[0]
    else:
        return tree_search(
            combiner(successors(states[0]), states[1:]),
            goal_test,
            successors,
            combiner)


def dfs(start, goal_test, successors):
    """Perform a depth first search on the state space."""
    return tree_search(start, goal_test, successors, append)


def bfs(start, goal_test, successors):
    """Perform a breadth first search on the state space."""
    return tree_search(start, goal_test, successors, append)
    
