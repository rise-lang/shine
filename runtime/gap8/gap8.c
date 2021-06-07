#include "../runtime.h"

extern void cluster_entry_point(void* args);

Context createDefaultContext(){
    ContextParams params = (ContextParams) pmsis_l2_malloc(sizeof(struct ContextParamsImpl))
    params->device_id = 0;
    return createContext(params);
}

/**
 * Ignore
*/
Context createContext(ContextParams params){
    struct pi_device cl_device;
    struct pi_cluster_conf cl_configuration;
    pi_cluster_conf_init(&cl_configuration);
    cl_configuration.id = params->device_id;
    pi_open_from_conf(&cl_device, &cl_configuration);
    if(pi_cluster_open(&cl_device))
    {
        printf("Cluster open failed\n");
        pmsis_exit(-1);
    }

    Context context = (Context) pmsis_l2_malloc(sizeof(struct ContextImpl));
    context->cl_device = cl_device;
    context->cl_configuration = cl_configuration;
    return context;
}

/**
 *
*/
void destroyContext(Context ctx){
    //pi_cluster_close(&(ctx->cl_device));

    //pmsis_l2_malloc_free(ctx->cl_configuration);
    //pmsis_l2_malloc_free(ctx->cl_device);
    //pmsis_l2_malloc_free(ctx);
    return;
}

/**
 * Ignore
*/
void waitFinished(Context ctx){
    return;
}

Kernel loadKernel(Context ctx, KernelParams params){
    struct pi_cluster_task * cl_task = pmsis_l2_malloc(sizeof(struct pi_cluster_task));
    memset(cl_task, 0, sizeof(struct pi_cluster_task));

    cl_task->entry = params->handler;
    cl_task->stack_size = params->stack_size;

    Kernel k = (Kernel) pmsis_l2_malloc(sizeof(struct KernelImpl));
    k->cl_task = cl_task;
    return k;
}


/**
 *
*/
void destroyKernel(Context ctx, Kernel k){
    pmsis_l2_malloc_free(k->cl_task, sizeof(k->cl_task));
    pmsis_l2_malloc_free(k, sizeof(k));
}

/***/
void launchKernel(Context ctx, Kernel k, LaunchConfig config, size_t arg_count, const KernelArg* args){
    k->cl_task->arg = args->value;
    pi_cluster_send_task_to_cl(&(ctx->cl_device), k->cl_task);
}
