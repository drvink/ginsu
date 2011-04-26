#ifndef MY_RSA_H
#define MY_RSA_H

#ifndef CONFIG_INCLUDED
#define CONFIG_INCLUDED
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#include "config.h"
#endif

#include <openssl/rsa.h>
#include <openssl/bn.h>
#include <openssl/evp.h>
#include <openssl/sha.h>
#include <limits.h>

#endif
