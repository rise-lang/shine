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

void destroyContext(Context ctx){
    pi_cluster_close(&(ctx->cl_device));

    pmsis_l2_malloc_free(&(ctx->cl_configuration), sizeof(struct pi_cluster_conf));
    pmsis_l2_malloc_free(&(ctx->cl_device), sizeof(struct pi_device));
    pmsis_l2_malloc_free(ctx, sizeof(struct ContextImpl));
    return;
}

void waitFinished(Context ctx){
    return;
}

Kernel loadKernel(void (*handler)(void*), uint32_t stack_size){
    struct pi_cluster_task * cl_task = pmsis_l2_malloc(sizeof(struct pi_cluster_task));
    memset(cl_task, 0, sizeof(struct pi_cluster_task));

    cl_task->entry = handler;
    cl_task->stack_size = stack_size;

    Kernel k = (Kernel) pmsis_l2_malloc(sizeof(struct KernelImpl));
    k->cl_task = cl_task;
    return k;
}

void destroyKernel(Context ctx, Kernel k){
    pmsis_l2_malloc_free(k->cl_task, sizeof(struct pi_cluster_task));
    pmsis_l2_malloc_free(k, sizeof(struct KernelImpl));
}

void launchKernel(Context ctx, Kernel k, int num_threads, void* args){

    k->cl_task->arg = args;
    pi_cluster_send_task_to_cl(&(ctx->cl_device), k->cl_task);

}
