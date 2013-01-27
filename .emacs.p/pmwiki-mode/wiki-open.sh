#!/bin/bash
#
# Christian Riddertröm
# $Id: wiki-open.sh,v 1.1 2004/10/30 20:35:24 mrchr Exp $
#
# Installation: Put this script somewhere in the path, and make sure
# that the first line points to a bash binary
#
S0=`basename $0`
if [[ "x$1" == "x" || "x$1" == "x--help" || "x$1" == "x-h" ]]; then
    cat <<EOF
Syntax: $S0 [options] wiki-page-address
Options:
  -h, --help		Show this help
  -v			Be more verbose
  -n			Only print commands, do not actually run
  -f function		Name of function to invoke (default: pmichaud-open)
  -e editor		Name of editor
  --version		Show version
  --client		Use client to connect to an existing Emacs

Example:
 $ $0 Main/HomePage		# Open Main.HomePage (using default function)

 $ $S0 -f lyx-open Main/HomePage # Use lyx-open to open page

 $ $S0 Main.HomePage -nw	# Pass extra option '-nw' to Emacs

Environment variables:

If the variable WIKI_OPEN_Function exists, it's value is taken as the
default function. E.g. setting 'WIKI_OPEN_Function=lyx-open' means you
don't have to give the options: '-l lyx-open' every time.

If the variable WIKI_OPEN_Args exists, it's value is prepended as
additional arguments send to the editor. E.g. setting
'WIKI_OPEN_Args=-nw' means you don't have to give the '-nw' option on
the command line.

EOF
    exit
fi

CVS_Rev='$Revision: 1.1 $'	# CVS Revision number
CVS_Rel='$Name:  $'		# CVS Release tag
if [[ "$1" == --version ]]; then
    Rev=${CVS_Rev:11:(${#CVS_Rev}>=13 ? ${#CVS_Rev}-13 : 0)}
    Rel=${CVS_Rel:  7:(${#CVS_Rel}>=9  ? ${#CVS_Rel}-9  : 0)}
    Rel=${Rel#release-} && Rel=${Rel//_/.};
    if [ -n "$Rel" ]; then
	printf "Release %s (revision of %s is %s)\n" "$Rel" "$S0" "$Rev"
    else
	printf "Revision of %s is %s\n" "$S0" "$Rev"
    fi;
    exit 0;
fi;

# Syntax: Error [errNo] [formatString [arguments]]
function Error(){
    err=1; (("$1")) && err=$1;	# Force evaluation of error argument
    if (($#0)); then		# Any arguments?
	(("$1")) && shift	# If 1st argument is a number, then shift args
	(($#)) && msg=("$1" "${@:2}"); # If msg.args. left, then set $msg
    fi;
    printf "Error %d in %s: ${msg[0]}\n" $err "`basename $0`" "${msg[@]:1}" >&2
    exit $err
}

# Syntax: Verbose [verbosityLevel] formatString [arguments]
Verbosity=0			# Default is Verbosity=0, i.e. no messages
function Verbose() {
    if ((${#1} == 1)); then vLevel=$1; shift; else vLevel=1; fi
    if (($Verbosity >= $vLevel)); then printf "$1\n" "${@:2}"; fi;
}

# Syntax: Log formatString [arguments]
function Log() {
    printf "$1\n" "${@:2}"
}

#
# Set default values of variables
#
DO=1;				# Controls is actions are actually DOne
opts=()				# Parsed options
pars=()				# Arguments excluding parsed options
quiet=0				# Be quiet?

#
# Script specific variables
EDITOR=${EDITOR:-emacs}		# Name of editor to invoke
CLIENT=gnuclient		# Client for attaching to editor
Adr=""
Function=${WIKI_OPEN_Function:-pmichaud-open} # Elisp function such as lyx-open
Args=${WIKI_OPEN_Args:-}	# Extra arguments for the editor
use_client=0;			# Use client to connect to Emacs

#
# Parse arguments
#
let i=1				# Index into parameters
while (($i <= $#)); do
    addOpt=0; addPar=0; addSkip=0; # N:o args to add to $opts/$pars or skip
    case "${@:$i:1}" in
	-q) addOpt=1; quiet=1;;
	-v) addOpt=1; let ++Verbosity;;
	+v) addOpt=1; let --Verbosity;;
	-d) addOpt=1; set -x;;
	-n) addOpt=1; DO=0;;
	+n) addOpt=1; DO=1;;
	-f) addOpt=2; Function=${@:(($i+1)):1};;
	-e) addOpt=2; EDITOR=${@:(($i+1)):1};;
	--test) addSkip=1; doTest=1;;
	--set-verbosity) addOpt=2; Verbosity=${@:(($i+1)):1};;
	--client) addOpt=1; use_client=1;;
	-*) Error "Unknown option '%s'" "${@:$i:1}";;
	*) Adr=${@:(($i)):1}; Args=("${Args[@]}" "${@:(($i+1))}"); break;
    esac
    (($addPar)) && pars=("${pars[@]}" "${@:$i:$addPar}"); 
    (($addOpt)) && opts=("${opts[@]}" "${@:$i:$addOpt}"); 
    (($addPar && $addOpt)) && Error 'Both $addPar and $addOpt are non-zero'
    ((!$addPar && !$addOpt && !$addSkip)) && \
	Error 'Both $addPar and $addOpt are zero'
    let i+=$addPar+$addOpt+$addSkip;
done

Verbose "Output is verbose (verbosity = %d)" "$Verbosity"
if (( $Verbosity >= 2 )); then
    printf "Variables in the script:\n"
    printf "DO=%s\n" "$DO"
    printf "EDITOR=%s\n" "$EDITOR"
    printf "CLIENT=%s\n" "$CLIENT"
    printf "Adr=%s\n" "$Adr"
    printf "Args=%s\n" "${Args[*]}"
    printf "Function=%s\n" "$Function"
    printf "\n"
fi;

# Verify validity of arguments
if [ -z "$Adr" ]; then Error "No (page) address given!"; fi;

# Execute proper code
msg="Not executing..."; (($DO)) && msg="Executing...";
Verbose 1 "%s" "$msg"
CMD=`printf "($Function \"%s\")" "$Adr"`;
if (( $use_client )); then
    Verbose 1 "%s" "$CLIENT -batch -eval \"$CMD\" ${Args[*]}"
    (($DO)) &&      $CLIENT -batch -eval  "$CMD" "${Args[@]}"
else
    Verbose 1 "%s" "$EDITOR --execute \"$CMD\" ${Args[*]}"
    (($DO)) &&      $EDITOR --execute  "$CMD" "${Args[@]}" 
fi;
