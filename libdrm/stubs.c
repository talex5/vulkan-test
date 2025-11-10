#include <sys/sysmacros.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>

value caml_libdrm_makedev(value v_maj, value v_min) {
	dev_t x = makedev(Long_val(v_maj), Long_val(v_min));
	return caml_copy_int64(x);
}

value caml_libdrm_major(value v_dev) { return Val_int(major(Int64_val(v_dev))); }
value caml_libdrm_minor(value v_dev) { return Val_int(minor(Int64_val(v_dev))); }
