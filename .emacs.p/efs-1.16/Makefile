###############################################################################
#
# File:         Makefile
# Release:      $EFS release: 1.16 $
# Release:      $dired release: 7.9 $
# Version:      $Revision: 1.5 $
# RCS:
# Description:  Makefile for byte-compiling EFS (primarily) and dired.
# Author:       Andy Norman, HPLabs, Bristol, UK.
# Created:      Sat Jan 30 00:18:56 1993
# Language:     Text (make script)
#
###############################################################################

## Edit these variables according to your configuration.

# Name of Emacs program
EMACS=xemacs
# Emacs version. This must be set to one of 18, 19, 19.23, 19.34
# l19.11, x19.15
EMACS_VERSION=x19.15
# Current working directory
CWD=`pwd`
# Directory in which to install the lisp files
LISPDIR=
# Directory in which to install the info files
INFODIR=
# Directory in which to install the Texinfo file
TEXIDIR=
# Directory containing byte-compiler.  This is used by fixup.el
BDIR=
# Directory containing Dired's .elc files.
DIREDDIR=
# Directory containing VM's .elc files.
VMDIR=
# Directory containing custom's .elc files.
CUSTOMDIR=
# Bourne shell executable, please.
SHELL=/bin/sh

###### It should not be necessary to edit anything below this line. ######

COREOBJS = efs-defun.elc efs-ovwrt.elc efs-fnh.elc efs-cu.elc efs-netrc.elc \
           efs.elc efs-dired.elc efs-report.elc \
           efs-cp-p.elc \
	   efs-dump.elc
CORESRC = efs-defun.el efs-ovwrt.el efs-fnh.el efs-cu.el efs-netrc.el \
          efs.el efs-dired.el efs-report.el \
          efs-cp-p.el \
	  efs-dump.el
DOBJS = dired.elc dired-mob.elc dired-oas.elc \
        dired-rgxp.elc dired-shell.elc dired-vir.elc dired-xy.elc \
        dired-grep.elc dired-uu.elc \
        dired-cmpr.elc dired-diff.elc dired-help.elc dired-sex.elc
DSRC = dired.el dired-mob.el dired-oas.el \
       dired-rgxp.el dired-shell.el dired-vir.el dired-xy.el \
       dired-grep.el dired-uu.el \
       dired-cmpr.el dired-diff.el dired-help.el dired-sex.el
EFSOBJS = $(COREOBJS) default-dir.elc efs-auto.elc \
          efs-cms.elc efs-cms-knet.elc efs-dos-distinct.elc efs-nos-ve.elc \
          efs-gwp.elc efs-kerberos.elc efs-hell.elc efs-ka9q.elc \
          efs-mpe.elc efs-mts.elc efs-mvs.elc efs-netware.elc \
          efs-pc.elc efs-ti-explorer.elc efs-ti-twenex.elc \
          efs-tops-20.elc efs-dl.elc efs-guardian.elc efs-coke.elc \
          efs-vms.elc efs-vos.elc efs-plan9.elc efs-ms-unix.elc
EFSSRC = $(CORESRC) default-dir.el efs-auto.el \
          efs-cms.el efs-cms-knet.el efs-dos-distinct.el efs-nos-ve.el \
          efs-gwp.el efs-kerberos.el efs-hell.el efs-ka9q.el \
          efs-mpe.el efs-mts.el efs-mvs.el efs-netware.el \
          efs-pc.el efs-ti-explorer.el efs-ti-twenex.el \
          efs-tops-20.el efs-dl.el efs-guardian.el efs-coke.el \
          efs-vms.el efs-vos.el efs-plan9.el efs-ms-unix.el
VMOBJS = efs-vm.elc
VMSRC = efs-vm.el
MULEOBJS_EFS = efs-dired-mule.elc
MULEOBJS_DIRED = dired-mule.elc
MULESRC_EFS = efs-dired-mule.el
MULESRC_DIRED = dired-mule.el
GEOBJS_EFS = passwd.elc auto-save.elc \
	 $(MULEOBJS_EFS)
GEOBJS_DIRED = dired-fsf.elc \
	 diff.elc \
	 $(MULEOBJS_DIRED)
GEOBJS = $(GEOBJS_EFS) $(GEOBJS_DIRED)
GESRC_EFS = passwd.el auto-save.el \
	$(MULESRC_EFS)
GESRC_DIRED = dired-fsf.el \
	diff.el\
	$(MULESRC_DIRED)
XEOBJS_EFS = $(VMOBJS)
XEOBJS_DIRED = dired-faces.elc dired-xemacs.elc diff.elc
XEOBJS = $(XEOBJS_EFS) $(XEOBJS_DIRED)
XESRC_EFS = $(VMSRC) $(MULESRC_EFS)
XESRC_DIRED = dired-faces.el dired-xemacs.el diff.el \
	$(MULESRC_DIRED)
XEPACKAGESRC_EFS = $(EFSSRC) efs-x19.15.el $(XESRC_EFS) $(MULESRC_EFS)
XEPACKAGEMISC_EFS = README ChangeLog LISTS efs.texi \
	            Makefile Makefile.efs \
	            dumped-lisp.el package-info.in
XEPACKAGESRC_DIRED = $(DSRC) $(XESRC_DIRED) $(MULESRC_DIRED)
XEPACKAGEMISC_DIRED = README ChangeLog LISTS \
	            Makefile Makefile.dired \
	            package-info.in
DISTSRC_EFS = $(EFSSRC) $(GESRC_EFS) $(XESRC_EFS) \
       efs-18.el efs-19.el efs-19.23.el efs-19.34.el \
       efs-l19.11.el efs-x19.15.el \
       emacs-19.el fn-handler.el \
       reporter.el fixup.el
DISTSRC_DIRED = $(DSRC) $(GESRC_DIRED) $(XESRC_DIRED) \
	fn-handler.el \
	reporter.el fixup.el
DISTMISC_EFS = README RELEASE INSTALL ChangeLog LISTS efs.texi \
	       Makefile.efs
DISTMISC_DIRED = README RELEASE INSTALL ChangeLog LISTS \
	         Makefile.dired
OBJS = $(DOBJS) $(EFSOBJS) $(VMOBJS) $(GEOBJS) $(XEOBJS) \
       efs-18.elc efs-19.elc efs-19.23.elc efs-19.34.elc \
       efs-l19.11.elc efs-x19.15.elc \
       emacs-19.elc fn-handler.elc \
       reporter.elc
INFOS = efs.aux efs.cp efs.dvi efs.fn efs.info efs.ky efs.log efs.pg \
        efs.toc efs.tp efs.vr

# fixup.el is never byte-compiled.  It would do no harm, but be a waste
# of time.

## Specify new rules.

.SUFFIXES: .elc .el .texi .info

.el.elc:
	BDIR=$(BDIR) CWD=$(CWD) DIREDDIR=$(DIREDDIR) VMDIR=$(VMDIR) CUSTOMDIR=$(CUSTOMDIR)\
  $(EMACS) -batch -no-site-file -l $(CWD)/fixup -f batch-byte-compile $(CWD)/$<

.texi.info:
	$(EMACS) -batch -f batch-texinfo-format $(CWD)/$<

## targets

# What lazy fingers buys you
default: efs-$(EMACS_VERSION)

# .elc files depend on .el source
# Do this in this brain-dead way because different makes do pattern
# rules differently. grumble grumble...
#
# dired
dired.elc: dired.el
dired-mob.elc: dired-mob.el
dired-oas.elc: dired-oas.el
dired-rgxp.elc: dired-rgxp.el
dired-shell.elc: dired-shell.el
dired-vir.elc: dired-vir.el
dired-xy.elc: dired-xy.el
dired-grep.elc: dired-grep.el
dired-uu.elc: dired-uu.el
dired-fsf.elc: dired-fsf.el
dired-cmpr.elc: dired-cmpr.el
dired-help.elc: dired-help.el
dired-diff.elc: dired-diff.el
dired-sex.elc: dired-sex.el
dired-mule.elc: dired-mule.el
dired-xemacs.elc: dired-xemacs.el
dired-faces.elc: dired-faces.el
default-dir.elc: default-dir.el
diff.elc: diff.el
# efs core files
efs.elc: efs.el
efs-defun.elc: efs-defun.el
efs-cp-p.elc: efs-cp-p.el
efs-cu.elc: efs-cu.el
efs-netrc.elc: efs-netrc.el
efs-auto.elc: efs-auto.el
efs-dired.elc: efs-dired.el
efs-dired-mule.elc: efs-dired-mule.el
efs-report.elc: efs-report.el
efs-ovwrt.elc: efs-ovwrt.el
efs-fnh.elc: efs-fnh.el
# efs multi-OS and FTP server support
efs-cms.elc: efs-cms.el
efs-cms-knet.elc: efs-cms-knet.el
efs-coke.elc: efs-coke.el
efs-dos-distinct.elc: efs-dos-distinct.el
efs-nos-ve.elc: efs-nos-ve.el
efs-gwp.elc:  efs-gwp.el
efs-hell.elc: efs-hell.el
efs-ka9q.elc: efs-ka9q.el
efs-kerberos.elc: efs-kerberos.el
efs-mpe.elc: efs-mpe.el
efs-mts.elc: efs-mts.el
efs-mvs.elc: efs-mvs.el
efs-netware.elc: efs-netware.el
efs-pc.elc: efs-pc.el
efs-ti-explorer.elc: efs-ti-explorer.el
efs-ti-twenex.elc: efs-ti-twenex.el
efs-tops-20.elc: efs-tops-20.el
efs-dl.elc: efs-dl.el
efs-vms.elc: efs-vms.el
efs-vos.elc: efs-vos.el
efs-guardian.elc: efs-guardian.el
efs-plan9.elc: efs-plan9.el
efs-ms-unix.elc: efs-ms-unix.el
# efs support for different Emacs versions
efs-18.elc: efs-18.el
efs-19.elc: efs-19.el
efs-19.23.elc: efs-19.23.el
efs-19.34.elc: efs-19.34.el
efs-l19.11.elc: efs-l19.11.el
efs-x19.15.elc: efs-x19.15.el
# efs vm support
efs-vm.elc: efs-vm.el
# backward compatibility files
fn-handler.elc: fn-handler.el
emacs-19.elc: emacs-19.el
# auto-save package
auto-save.elc: auto-save.el

# Core targets
core: $(COREOBJS)

# Extra perks
auto: core efs-auto.elc
cms: core efs-cms.elc
cms-knet: core efs-cms-knet.elc
dos-distinct: core efs-dos-distinct.elc
nos-ve: core efs-nos-ve.elc
gwp: core efs-gwp.elc
hell: core efs-hell.elc
ka9q: core efs-ka9q.elc
kerberos: core efs-kerberos.elc
mpe: core efs-mpe.elc
mts: core efs-mts.elc
mvs: core efs-mvs.elc
netware: core efs-netware.elc
pc: core efs-pc.elc
ti-explorer: core efs-ti-explorer.elc
ti-twenex: core efs-ti-twenex.elc
tops-20: core efs-tops-20.elc
dl: core efs-dl.elc
vms: core efs-vms.elc
vos: core efs-vos.elc
guardian: core efs-guardian.elc
plan9: core efs-plan9.elc
coke: core efs-coke.elc
vm: core $(VMOBJS)

# The grand tour
efs: $(EFSOBJS)
dired: $(DOBJS)
info: efs.info

# Making for a specific emacs version
efs-18: emacs-19.elc fn-handler.elc efs efs-18.elc \
    efs-dired-mule.elc reporter.elc passwd.elc auto-save.elc
efs-19: fn-handler.elc efs efs-19.elc $(GEOBJS_EFS)
efs-19.23: efs efs-19.23.elc $(GEOBJS_EFS)
efs-19.34: efs efs-19.34.elc $(GEOBJS_EFS)
efs-l19.11: efs efs-l19.11.elc $(XEOBJS_EFS)
efs-x19.15: efs efs-x19.15.elc $(XEOBJS_EFS)
efs-x20: efs efs-x19.15.elc $(XEOBJS_EFS) $(MULEOBJS_EFS)

dired-18: emacs-19.elc fn-handler.elc efs dired dired-mule.elc \
    reporter.elc diff.elc
dired-19: fn-handler.elc efs dired $(GEOBJS_DIRED)
dired-19.23: dired $(GEOBJS_DIRED)
dired-19.34: dired $(GEOBJS_DIRED)
dired-l19.11: dired $(XEOBJS_DIRED)
dired-x19.15: dired $(XEOBJS_DIRED)
dired-x20: dired $(XEOBJS_DIRED) $(MULEOBJS_DIRED)

# Installation
install_obj:
	@echo "Installing in $(LISPDIR) and $(INFODIR)..."
	cp *.elc $(LISPDIR)
	cp *.info $(INFODIR)
install_src:
	@echo "Installing in $(LISPDIR)..."
	cp `ls *.el | grep -v "fixup"` $(LISPDIR)
	cp *texi $(TEXIDIR)

efs_package_dist:
	@echo "Installing in $(STAGING)..."
	cp $(XEPACKAGESRC_EFS) $(XEPACKAGEMISC_EFS) $(STAGING)
	cp kill-revs $(STAGING)
	cd $(STAGING) ; $(SHELL) ./kill-revs ; rm -f kill-revs

efs_dist:
	@echo "Installing in $(STAGING)..."
	cp $(DISTSRC_EFS) $(DISTMISC_EFS) $(STAGING)
	cd $(STAGING); mv Makefile.efs Makefile

dired_package_dist:
	@echo "Installing in $(STAGING)..."
	cp $(XEPACKAGESRC_DIRED) $(XEPACKAGEMISC_DIRED) $(STAGING)
	cp kill-revs $(STAGING)
	cd $(STAGING) ; $(SHELL) ./kill-revs ; rm -f kill-revs

dired_dist:
	@echo "Installing in $(STAGING)..."
	cp $(DISTSRC_DIRED) $(DISTMISC_DIRED) $(STAGING)
	cd $(STAGING); mv Makefile.dired Makefile
clean::
	rm -f $(OBJS) $(INFOS)

## end of Makefile ##
