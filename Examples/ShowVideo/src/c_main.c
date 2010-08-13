#include <stdio.h>
#include "HsFFI.h"
 
#include <SDL.h>
 
#ifdef __GLASGOW_HASKELL__
#include "SDLWrapper_stub.h"
extern void __stginit_SDLWrapper ( void );
#endif
 
int main(int argc, char* args[])
{
    int i;

    hs_init(&argc, &args);
#ifdef __GLASGOW_HASKELL__
    hs_add_root(__stginit_SDLWrapper);
#endif
 
    haskell_main();
 
    hs_exit();

    return 0;
}
