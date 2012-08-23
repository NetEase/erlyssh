#include <stdio.h>
#include "erl_driver.h"
#include <readline/history.h>
#include <readline/readline.h>

typedef struct {
    ErlDrvPort port;
} readline_data;

static char *line_read = (char*)NULL;
static char *prompt = "essh>: ";

static ErlDrvData libreadlinedrv_start(ErlDrvPort port, char *buff){
	
    readline_data* d = (readline_data*)driver_alloc(sizeof(readline_data));
    d->port = port;	
    return (ErlDrvData)d;
}

static void libreadlinedrv_stop(ErlDrvData handle)
{
    driver_free((char*)handle);
}

static void libreadlinedrv_output(ErlDrvData handle, char *buff, int bufflen)
{
    readline_data* d = (readline_data*)handle;

    if (line_read)
    {
      free(line_read);
      line_read = (char *)NULL;
    }

	line_read = readline(prompt);

	if (line_read && *line_read)
        add_history(line_read);

    driver_output(d->port, line_read, strlen(line_read));
}

ErlDrvEntry readline_driver_entry = {
    NULL,                       /* F_PTR init, N/A */
    libreadlinedrv_start,          /* L_PTR start, called when port is opened */
    libreadlinedrv_stop,           /* F_PTR stop, called when port is closed */
    libreadlinedrv_output,         /* F_PTR output, called when erlang has sent */
    NULL,                       /* F_PTR ready_input, called when input descriptor ready */
    NULL,                       /* F_PTR ready_output, called when output descriptor ready */
    "libreadlinedrv",              /* char *driver_name, the argument to open_port */
    NULL,                       /* F_PTR finish, called when unloaded */
    NULL,                       /* F_PTR control, port_command callback */
    NULL,                       /* F_PTR timeout, reserved */
    NULL                        /* F_PTR outputv, reserved */
};

DRIVER_INIT(libreadlinedrv) /* must match name in driver_entry */
{
    return &readline_driver_entry;
}
