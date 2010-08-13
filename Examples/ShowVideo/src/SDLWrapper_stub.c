#define IN_STG_CODE 0
#include "Rts.h"
#include "Stg.h"
#ifdef __cplusplus
extern "C" {
#endif
 
extern StgClosure SDLWrapper_zdfmainzua1Pk_closure;
void haskell_main(void)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,&SDLWrapper_zdfmainzua1Pk_closure) ,&ret);
rts_checkSchedStatus("haskell_main",cap);
rts_unlock(cap);
}
static void stginit_export_SDLWrapper_zdfmainzua1Pk() __attribute__((constructor));
static void stginit_export_SDLWrapper_zdfmainzua1Pk()
{getStablePtr((StgPtr) &SDLWrapper_zdfmainzua1Pk_closure);}
#ifdef __cplusplus
}
#endif

