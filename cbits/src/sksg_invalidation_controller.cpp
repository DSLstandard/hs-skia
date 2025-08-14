#include "skia_capi/sksg_invalidation_controller.h"

#include "common_include_pch.hpp"

sksg_invalidation_controller_t* sksg_invalidation_controller_new(void) {
    return ToSksgInvalidationController(new sksg::InvalidationController());
}

void sksg_invalidation_controller_delete(sksg_invalidation_controller_t* instance) {
    delete AsSksgInvalidationController(instance);
}

void sksg_invalidation_controller_inval(sksg_invalidation_controller_t* instance, sk_rect_t* rect, sk_matrix_t* matrix) {
    AsSksgInvalidationController(instance)->inval(*AsRect(rect), AsMatrix(matrix));
}

void sksg_invalidation_controller_get_bounds(sksg_invalidation_controller_t* instance, sk_rect_t* bounds) {
    *bounds = ToRect(AsSksgInvalidationController(instance)->bounds());
}

void sksg_invalidation_controller_begin(sksg_invalidation_controller_t* instance) {
    AsSksgInvalidationController(instance)->begin();
}

void sksg_invalidation_controller_end(sksg_invalidation_controller_t* instance) {
    AsSksgInvalidationController(instance)->end();
}

void sksg_invalidation_controller_reset(sksg_invalidation_controller_t* instance) {
    AsSksgInvalidationController(instance)->reset();
}