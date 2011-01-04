lib_LTLIBRARIES = libexplor.la
libexplor_la_SOURCES = mini-explor.f
libexplor_la_LDFLAGS = -shared -fPIC

dist_bin_SCRIPTS = explor 
CLEANFILES = $(bin_SCRIPTS)
extra_dist = explor.in
dist_pkgdata_DATA = mini-explor.f end.f


explor_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	 	   -e 's,[@]pkgdatadir[@],$(pkgdatadir),g' \
		   -e 's,[@]fortran[@],$(F77),g'

explor: explor.in
	$(explor_subst) < $(srcdir)/explor.in > explor
	chmod +x explor
