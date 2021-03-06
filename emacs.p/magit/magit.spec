Name:		magit
Version:	1.2.0-183-gadf5164
Release:	1%{?dist}
Summary:	An Emacs Extension for Git
BuildArch: noarch
Group:		Development/Tools
License:	GPLv3
URL:		http://magit.github.com/magit/
Source0:	%{url}/%{name}-%{version}.tar.gz
BuildRoot:	%{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

BuildRequires:	texinfo, autoconf, automake, emacs
Requires:	git, emacs

%description
Magit is an interface to the version control system Git, implemented
as an extension to Emacs.

%prep
%setup -q

%build
%configure --prefix=/usr --sysconfdir=/etc --with-site-start=/usr/share/emacs/site-lisp/site-start.d
make %{?_smp_mflags}


%install
make install DESTDIR=$RPM_BUILD_ROOT
rm $RPM_BUILD_ROOT/usr/share/info/dir

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
/usr/share/emacs/site-lisp/site-start.d/50magit.el
/usr/share/emacs/site-lisp/magit.el
/usr/share/emacs/site-lisp/magit.elc
%doc README.md
/usr/share/info/magit.info.gz

%changelog
* Sun Mar 14 2010	Ben Walton <bwalton@artsci.utoronto.ca>
- Initial spec file creation.
