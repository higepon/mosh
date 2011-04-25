%define						short_name			mosh
Summary:                 A Fast R6RS Scheme interpreter.
Name:                    %{short_name}-scheme
Version:                 0.2.6
Release:                 1%{?dist}


Source0:                 http://%{short_name}-scheme.googlecode.com/files/%{short_name}-%{version}.tar.gz
Patch1:                  mosh-scheme.configure.ac.patch
Group:                   Development/Languages
License:                 BSD
URL:                     https://github.com/higepon/%{short_name}
BuildRoot:               %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
Requires:                gmp, oniguruma, mysql-libs


%description
Mosh is a free and fast interpreter for Scheme as specified in the R6RS.
(R6RS is the latest revision of the Scheme standard)
The current release of Mosh supports all of the features of R6RS.

Features
*	Mosh offers following functions for development
*	Library system
*	Macro
*	Records
*	Condition system
*	Exception and error handling
*	Unicode
*	Bytevector
*	Hashtable
*	Enumration
*	Database
*	Foreign Function Interface
*	Socket
*	CGI

%prep
%setup -q -n %{short_name}-%{version}
%patch1

%build
# %{__make} PREFIX=%{_prefix} DESTDIR=%{RPM_BUILD_ROOT}  LIBDIR=%{_libdir} SOLIBDIR=%{_libdir} MODDIR=%{_datarootdir}/chibi-scheme doc all
%{configure}  --with-mysql
%{__make}


%install
rm -rf $RPM_BUILD_ROOT
mkdir -p ${RPM_BUILD_ROOT}

%{__make} "DESTDIR=${RPM_BUILD_ROOT}" install


# %{__make} PREFIX=%{_prefix} DESTDIR=${RPM_BUILD_ROOT} LIBDIR=%{_libdir} SOLIBDIR=%{_libdir} LDFLAGS="-C ${RPM_BUILD_ROOT}%{_sysconfdir}/ld.so.conf.d" MODDIR=%{_datarootdir}/chibi-scheme  install

%clean
# rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{_bindir}/mosh
%{_bindir}/nmosh
%{_bindir}/mosh_config
%{_datarootdir}/%{name}-%{version}
%{_datarootdir}/man



%changelog
* Wed Apr 23 2011 Rajesh Krishnan <devel[AT]krishnan.cc> - 0.2.6
- Initial release


