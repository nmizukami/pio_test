/** @file
 *
 * This is the template for the config.h file, which is created at
 * build-time by cmake.
 */
#ifndef _PIO_CONFIG_
#define _PIO_CONFIG_

/** The major part of the version number. */
#define PIO_VERSION_MAJOR 2

/** The minor part of the version number. */
#define PIO_VERSION_MINOR 4

/** The patch part of the version number. */
#define PIO_VERSION_PATCH 0

/** Set to non-zero to use native malloc. By defauly the PIO library
 * will use the included bget() package for memory management. */
#define PIO_USE_MALLOC 0

/** Set to non-zero to turn on logging. Output may be large. */
#define PIO_ENABLE_LOGGING 0

/** Size of MPI_Offset type. */
#define SIZEOF_MPI_OFFSET 8

/* buffer size for darray data. */
#define PIO_BUFFER_SIZE 134217728

#define USE_VARD 0

#endif /* _PIO_CONFIG_ */
