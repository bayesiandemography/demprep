


setGeneric("make_labels",
           function(object) {
               labels <- object@labels
               include_na <- object@include_na
               make_labels_default(labels = labels,
                                   include_na = include_na)
           })
