#!/bin/bash
if test $# != 0
then
	if test -d $1
	then
		for ff in $1/* $1/*/*
		do
			if test -h "$ff"
			then
#				echo "$ff is a symlink"
				repl=$(readlink "$ff")
				rm "$ff"
				cp -a "$repl" "$ff"
			fi
		done
	fi
fi
