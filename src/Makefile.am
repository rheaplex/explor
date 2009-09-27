#pkglib_LIBRARIES = libexplor.a
#libexplor_a_SOURCES = mini-explor.f
#libexplor_a_LIBADD = $(FLIBS)

dist_bin_SCRIPTS = explor 
CLEANFILES = $(bin_SCRIPTS)
extra_dist = explor.in
dist_pkgdata_DATA = mini-explor.f

explor_subst = sed -e 's,[@]pkgdatadir[@],$(pkgdatadir),g' \
		   -e 's,[@]fortran[@],$(F77),g'

explor: explor.in
	$(explor_subst) < $(srcdir)/explor.in > explor
	chmod +x explor
