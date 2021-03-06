#include "jsapi.h"

/* The class of the global object. */
static JSClass global_class = { "global",
                                JSCLASS_NEW_RESOLVE | JSCLASS_GLOBAL_FLAGS,
                                JS_PropertyStub,
                                JS_DeletePropertyStub,
                                JS_PropertyStub,
                                JS_StrictPropertyStub,
                                JS_EnumerateStub,
                                JS_ResolveStub,
                                JS_ConvertStub,
                                nullptr,
                                JSCLASS_NO_OPTIONAL_MEMBERS
};

/* The error reporter callback. */
void reportError(JSContext *cx, const char *message, JSErrorReport *report) {
     fprintf(stderr, "%s:%u:%s\n",
             report->filename ? report->filename : "[no filename]",
             (unsigned int) report->lineno,
             message);
}

int run(JSContext *cx) {
    /* Enter a request before running anything in the context */
    JSAutoRequest ar(cx);

    /* Create the global object in a new compartment. */
    JSObject *global = JS_NewGlobalObject(cx, &global_class, nullptr);
    if (!global)
        return 1;
  
    /* These should indicate source location for diagnostics. */
    char *filename;
    unsigned int lineno;

    /*
     * The return value comes back here -- if it could be a GC thing, you must
     * add it to the GC's "root set" with JS_AddRoot(cx, &thing) where thing
     * is a JSString *, JSObject *, or jsdouble *, and remove the root before
     * rval goes out of scope, or when rval is no longer needed.
     */
    jsval rval;
    JSBool ok;

    /*
     * Some example source in a C string.  Larger, non-null-terminated buffers
     * can be used, if you pass the buffer length to JS_EvaluateScript.
     */
    static char source[] = "6 * 7";

    double d;

    /* Set the context's global */
    JSAutoCompartment ac(cx, global);
    JS_SetGlobalObject(cx, global);

    /* Populate the global object with the standard globals, like Object and Array. */
    if (!JS_InitStandardClasses(cx, global))
        return 1;

    ok = JS_EvaluateScript(cx, global, source, strlen(source),
                           filename, lineno, &rval);
    if (ok) {
        /* Should get a number back from the example source. */
        ok = JS_ValueToNumber(cx, rval, &d);

        if (ok)
           fprintf(stderr,"Calculated: %f\n",d);
    }
    return 0;
}

int main(int argc, const char *argv[]) {
    /* Initialize the JS engine -- new/required as of SpiderMonkey 31. */
    /* if (!JS_Init())
       return 1; */

    /* Create a JS runtime. */
    JSRuntime *rt = JS_NewRuntime(8L * 1024L * 1024L, JS_NO_HELPER_THREADS);
    if (!rt)
       return 1;

    /* Create a context. */
    JSContext *cx = JS_NewContext(rt, 8192);
    if (!cx)
       return 1;

    JS_SetOptions(cx, JSOPTION_VAROBJFIX);
    JS_SetErrorReporter(cx, reportError);

    int status = run(cx);

    JS_DestroyContext(cx);
    JS_DestroyRuntime(rt);

    /* Shut down the JS engine. */
    JS_ShutDown();

    return status;
}
