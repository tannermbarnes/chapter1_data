SET(OpenBLAS_VERSION "0.3.26")
SET(OpenBLAS_INCLUDE_DIRS "${CMAKE_CURRENT_LIST_DIR}/../../../include/openblas")
SET(OpenBLAS_LIBRARIES -lopenblas -lpthread -fopenmp -lgfortran -lquadmath )
