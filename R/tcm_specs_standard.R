tcm_specs_standard = function () 
{
  data.table(tcm_id = c("standard_ref_1", "standard_ref_2", 
                        "standard_ref_3", "standard_ref_4"), type = c("ext", 
                                                                      "ext", "ext", "int"), window_size = c(5, 10, 110, Inf))
}
