//Proto
#include "gap8.h"

Context createDefaultContext(){
    return createContext(0);
}

Context createContext(int device_id){
    struct pi_device cl_device;
    struct pi_cluster_conf cl_configuration;
    pi_cluster_conf_init(&cl_configuration);
    cl_configuration.id = device_id;
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

/**
 * Not actually loading Kernel from source, but NVM
*/
Kernel loadKernel(void (*handler)(void*), size_t stack_size){
    struct pi_cluster_task * cl_task = pmsis_l2_malloc(sizeof(struct pi_cluster_task));
    memset(cl_task, 0, sizeof(struct pi_cluster_task));
    
    /**
     * Both of these entries are hardcoded.
     * Existance of void cluster_entry_point(void* args); is presumed
    */
    cl_task->entry = handler;
    cl_task->stack_size = (uint32_t) stack_size;

    Kernel k = (Kernel) pmsis_l2_malloc(sizeof(struct KernelImpl));
    k->cl_task = cl_task;
    return k;
}

/**
 * 
*/
void destroyKernel(Context ctx, Kernel k){
    //pmsis_l2_malloc_free(k->cl_task, sizeof(k->cl_task));
    //pmsis_l2_malloc_free(k, sizeof(k));
}

void launchKernel(
  Context ctx, Kernel k,
  int num_threads,
  size_t arg_count, const KernelArg* args){

    k->cl_task->arg = args->value;
    pi_cluster_send_task_to_cl(&(ctx->cl_device), k->cl_task);

}
