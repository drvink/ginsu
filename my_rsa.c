#include "my_rsa.h"
#include <HsFFI.h>
HsInt evpCipherContextBlockSize(EVP_CIPHER_CTX *e) {return EVP_CIPHER_block_size(EVP_CIPHER_CTX_cipher(e));}
EVP_PKEY *pkeyNewRSA(RSA *rsa) {EVP_PKEY *pk; pk = EVP_PKEY_new(); EVP_PKEY_assign_RSA(pk, rsa); return pk;}
HsFunPtr  get_KEY (void) {return (HsFunPtr)&EVP_PKEY_free;}
