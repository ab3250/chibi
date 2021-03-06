/*
  File autogenerated by gengetopt 
  generated with the following command:
  ../src/gengetopt --gen-version --input ./canonize-names-cmd.ggo --func-name canonize-names-cmd-parser --file-name ../tests/canonize-names-cmd 

  The developers of gengetopt consider the fixed text that goes in all
  gengetopt output files to be in the public domain:
  we make no copyright claims on it.
*/

/* If we use autoconf.  */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef FIX_UNUSED
#define FIX_UNUSED(X) (void) (X) /* avoid warnings for unused params */
#endif

#include <getopt.h>

#include "../tests/canonize-names-cmd.h"

const char *gengetopt_args_info_purpose = "";

const char *gengetopt_args_info_usage = "Usage: canonize_names [OPTION]...";

const char *gengetopt_args_info_versiontext = "";

const char *gengetopt_args_info_description = "";

const char *gengetopt_args_info_help[] = {
  "  -h, --help         Print help and exit",
  "  -V, --version      Print version and exit",
  "  -i, --foo-bar=INT  foo-bar option",
  "  -j, --foo.foo=INT  foo.foo option",
    0
};

typedef enum {ARG_NO
  , ARG_INT
} canonize_names_cmd_parser_arg_type;

static
void clear_given (struct gengetopt_args_info *args_info);
static
void clear_args (struct gengetopt_args_info *args_info);

static int
canonize_names_cmd_parser_internal (int argc, char **argv, struct gengetopt_args_info *args_info,
                        struct canonize_names_cmd_parser_params *params, const char *additional_error);


static char *
gengetopt_strdup (const char *s);

static
void clear_given (struct gengetopt_args_info *args_info)
{
  args_info->help_given = 0 ;
  args_info->version_given = 0 ;
  args_info->foo_bar_given = 0 ;
  args_info->foo_foo_given = 0 ;
  args_info->my_group_group_counter = 0 ;
  args_info->my_group2_group_counter = 0 ;
  args_info->my_group3_group_counter = 0 ;
}

static
void clear_args (struct gengetopt_args_info *args_info)
{
  FIX_UNUSED (args_info);
  args_info->foo_bar_orig = NULL;
  args_info->foo_foo_orig = NULL;
  
}

static
void init_args_info(struct gengetopt_args_info *args_info)
{


  args_info->help_help = gengetopt_args_info_help[0] ;
  args_info->version_help = gengetopt_args_info_help[1] ;
  args_info->foo_bar_help = gengetopt_args_info_help[2] ;
  args_info->foo_foo_help = gengetopt_args_info_help[3] ;
  
}

void
canonize_names_cmd_parser_print_version (void)
{
  printf ("%s %s\n",
     (strlen(CANONIZE_NAMES_CMD_PARSER_PACKAGE_NAME) ? CANONIZE_NAMES_CMD_PARSER_PACKAGE_NAME : CANONIZE_NAMES_CMD_PARSER_PACKAGE),
     CANONIZE_NAMES_CMD_PARSER_VERSION);

  if (strlen(gengetopt_args_info_versiontext) > 0)
    printf("\n%s\n", gengetopt_args_info_versiontext);
}

static void print_help_common(void)
{
	size_t len_purpose = strlen(gengetopt_args_info_purpose);
	size_t len_usage = strlen(gengetopt_args_info_usage);

	if (len_usage > 0) {
		printf("%s\n", gengetopt_args_info_usage);
	}
	if (len_purpose > 0) {
		printf("%s\n", gengetopt_args_info_purpose);
	}

	if (len_usage || len_purpose) {
		printf("\n");
	}

	if (strlen(gengetopt_args_info_description) > 0) {
		printf("%s\n\n", gengetopt_args_info_description);
	}
}

void
canonize_names_cmd_parser_print_help (void)
{
  int i = 0;
  print_help_common();
  while (gengetopt_args_info_help[i])
    printf("%s\n", gengetopt_args_info_help[i++]);
}

void
canonize_names_cmd_parser_init (struct gengetopt_args_info *args_info)
{
  clear_given (args_info);
  clear_args (args_info);
  init_args_info (args_info);
}

void
canonize_names_cmd_parser_params_init(struct canonize_names_cmd_parser_params *params)
{
  if (params)
    { 
      params->override = 0;
      params->initialize = 1;
      params->check_required = 1;
      params->check_ambiguity = 0;
      params->print_errors = 1;
    }
}

struct canonize_names_cmd_parser_params *
canonize_names_cmd_parser_params_create(void)
{
  struct canonize_names_cmd_parser_params *params = 
    (struct canonize_names_cmd_parser_params *)malloc(sizeof(struct canonize_names_cmd_parser_params));
  canonize_names_cmd_parser_params_init(params);  
  return params;
}

static void
free_string_field (char **s)
{
  if (*s)
    {
      free (*s);
      *s = 0;
    }
}


static void
canonize_names_cmd_parser_release (struct gengetopt_args_info *args_info)
{

  free_string_field (&(args_info->foo_bar_orig));
  free_string_field (&(args_info->foo_foo_orig));
  
  

  clear_given (args_info);
}


static void
write_into_file(FILE *outfile, const char *opt, const char *arg, const char *values[])
{
  FIX_UNUSED (values);
  if (arg) {
    fprintf(outfile, "%s=\"%s\"\n", opt, arg);
  } else {
    fprintf(outfile, "%s\n", opt);
  }
}


int
canonize_names_cmd_parser_dump(FILE *outfile, struct gengetopt_args_info *args_info)
{
  int i = 0;

  if (!outfile)
    {
      fprintf (stderr, "%s: cannot dump options to stream\n", CANONIZE_NAMES_CMD_PARSER_PACKAGE);
      return EXIT_FAILURE;
    }

  if (args_info->help_given)
    write_into_file(outfile, "help", 0, 0 );
  if (args_info->version_given)
    write_into_file(outfile, "version", 0, 0 );
  if (args_info->foo_bar_given)
    write_into_file(outfile, "foo-bar", args_info->foo_bar_orig, 0);
  if (args_info->foo_foo_given)
    write_into_file(outfile, "foo.foo", args_info->foo_foo_orig, 0);
  

  i = EXIT_SUCCESS;
  return i;
}

int
canonize_names_cmd_parser_file_save(const char *filename, struct gengetopt_args_info *args_info)
{
  FILE *outfile;
  int i = 0;

  outfile = fopen(filename, "w");

  if (!outfile)
    {
      fprintf (stderr, "%s: cannot open file for writing: %s\n", CANONIZE_NAMES_CMD_PARSER_PACKAGE, filename);
      return EXIT_FAILURE;
    }

  i = canonize_names_cmd_parser_dump(outfile, args_info);
  fclose (outfile);

  return i;
}

void
canonize_names_cmd_parser_free (struct gengetopt_args_info *args_info)
{
  canonize_names_cmd_parser_release (args_info);
}

/** @brief replacement of strdup, which is not standard */
char *
gengetopt_strdup (const char *s)
{
  char *result = 0;
  if (!s)
    return result;

  result = (char*)malloc(strlen(s) + 1);
  if (result == (char*)0)
    return (char*)0;
  strcpy(result, s);
  return result;
}

int
canonize_names_cmd_parser (int argc, char **argv, struct gengetopt_args_info *args_info)
{
  return canonize_names_cmd_parser2 (argc, argv, args_info, 0, 1, 1);
}

int
canonize_names_cmd_parser_ext (int argc, char **argv, struct gengetopt_args_info *args_info,
                   struct canonize_names_cmd_parser_params *params)
{
  int result;
  result = canonize_names_cmd_parser_internal (argc, argv, args_info, params, 0);

  if (result == EXIT_FAILURE)
    {
      canonize_names_cmd_parser_free (args_info);
      exit (EXIT_FAILURE);
    }
  
  return result;
}

int
canonize_names_cmd_parser2 (int argc, char **argv, struct gengetopt_args_info *args_info, int override, int initialize, int check_required)
{
  int result;
  struct canonize_names_cmd_parser_params params;
  
  params.override = override;
  params.initialize = initialize;
  params.check_required = check_required;
  params.check_ambiguity = 0;
  params.print_errors = 1;

  result = canonize_names_cmd_parser_internal (argc, argv, args_info, &params, 0);

  if (result == EXIT_FAILURE)
    {
      canonize_names_cmd_parser_free (args_info);
      exit (EXIT_FAILURE);
    }
  
  return result;
}

int
canonize_names_cmd_parser_required (struct gengetopt_args_info *args_info, const char *prog_name)
{
  FIX_UNUSED (args_info);
  FIX_UNUSED (prog_name);
  return EXIT_SUCCESS;
}


static char *package_name = 0;

/**
 * @brief updates an option
 * @param field the generic pointer to the field to update
 * @param orig_field the pointer to the orig field
 * @param field_given the pointer to the number of occurrence of this option
 * @param prev_given the pointer to the number of occurrence already seen
 * @param value the argument for this option (if null no arg was specified)
 * @param possible_values the possible values for this option (if specified)
 * @param default_value the default value (in case the option only accepts fixed values)
 * @param arg_type the type of this option
 * @param check_ambiguity @see canonize_names_cmd_parser_params.check_ambiguity
 * @param override @see canonize_names_cmd_parser_params.override
 * @param no_free whether to free a possible previous value
 * @param multiple_option whether this is a multiple option
 * @param long_opt the corresponding long option
 * @param short_opt the corresponding short option (or '-' if none)
 * @param additional_error possible further error specification
 */
static
int update_arg(void *field, char **orig_field,
               unsigned int *field_given, unsigned int *prev_given, 
               char *value, const char *possible_values[],
               const char *default_value,
               canonize_names_cmd_parser_arg_type arg_type,
               int check_ambiguity, int override,
               int no_free, int multiple_option,
               const char *long_opt, char short_opt,
               const char *additional_error)
{
  char *stop_char = 0;
  const char *val = value;
  int found;
  FIX_UNUSED (field);

  stop_char = 0;
  found = 0;

  if (!multiple_option && prev_given && (*prev_given || (check_ambiguity && *field_given)))
    {
      if (short_opt != '-')
        fprintf (stderr, "%s: `--%s' (`-%c') option given more than once%s\n", 
               package_name, long_opt, short_opt,
               (additional_error ? additional_error : ""));
      else
        fprintf (stderr, "%s: `--%s' option given more than once%s\n", 
               package_name, long_opt,
               (additional_error ? additional_error : ""));
      return 1; /* failure */
    }

  FIX_UNUSED (default_value);
    
  if (field_given && *field_given && ! override)
    return 0;
  if (prev_given)
    (*prev_given)++;
  if (field_given)
    (*field_given)++;
  if (possible_values)
    val = possible_values[found];

  switch(arg_type) {
  case ARG_INT:
    if (val) *((int *)field) = strtol (val, &stop_char, 0);
    break;
  default:
    break;
  };

  /* check numeric conversion */
  switch(arg_type) {
  case ARG_INT:
    if (val && !(stop_char && *stop_char == '\0')) {
      fprintf(stderr, "%s: invalid numeric value: %s\n", package_name, val);
      return 1; /* failure */
    }
    break;
  default:
    ;
  };

  /* store the original value */
  switch(arg_type) {
  case ARG_NO:
    break;
  default:
    if (value && orig_field) {
      if (no_free) {
        *orig_field = value;
      } else {
        if (*orig_field)
          free (*orig_field); /* free previous string */
        *orig_field = gengetopt_strdup (value);
      }
    }
  };

  return 0; /* OK */
}


int
canonize_names_cmd_parser_internal (
  int argc, char **argv, struct gengetopt_args_info *args_info,
                        struct canonize_names_cmd_parser_params *params, const char *additional_error)
{
  int c;	/* Character of the parsed option.  */

  int error_occurred = 0;
  struct gengetopt_args_info local_args_info;
  
  int override;
  int initialize;
  int check_required;
  int check_ambiguity;
  
  package_name = argv[0];
  
  /* TODO: Why is this here? It is not used anywhere. */
  override = params->override;
  FIX_UNUSED(override);

  initialize = params->initialize;
  check_required = params->check_required;

  /* TODO: Why is this here? It is not used anywhere. */
  check_ambiguity = params->check_ambiguity;
  FIX_UNUSED(check_ambiguity);

  if (initialize)
    canonize_names_cmd_parser_init (args_info);

  canonize_names_cmd_parser_init (&local_args_info);

  optarg = 0;
  optind = 0;
  opterr = params->print_errors;
  optopt = '?';

  while (1)
    {
      int option_index = 0;

      static struct option long_options[] = {
        { "help",	0, NULL, 'h' },
        { "version",	0, NULL, 'V' },
        { "foo-bar",	1, NULL, 'i' },
        { "foo.foo",	1, NULL, 'j' },
        { 0,  0, 0, 0 }
      };

      c = getopt_long (argc, argv, "hVi:j:", long_options, &option_index);

      if (c == -1) break;	/* Exit from `while (1)' loop.  */

      switch (c)
        {
        case 'h':	/* Print help and exit.  */
          canonize_names_cmd_parser_print_help ();
          canonize_names_cmd_parser_free (&local_args_info);
          exit (EXIT_SUCCESS);

        case 'V':	/* Print version and exit.  */
          canonize_names_cmd_parser_print_version ();
          canonize_names_cmd_parser_free (&local_args_info);
          exit (EXIT_SUCCESS);

        case 'i':	/* foo-bar option.  */
        
        
          if (update_arg( (void *)&(args_info->foo_bar_arg), 
               &(args_info->foo_bar_orig), &(args_info->foo_bar_given),
              &(local_args_info.foo_bar_given), optarg, 0, 0, ARG_INT,
              check_ambiguity, override, 0, 0,
              "foo-bar", 'i',
              additional_error))
            goto failure;
        
          break;
        case 'j':	/* foo.foo option.  */
        
        
          if (update_arg( (void *)&(args_info->foo_foo_arg), 
               &(args_info->foo_foo_orig), &(args_info->foo_foo_given),
              &(local_args_info.foo_foo_given), optarg, 0, 0, ARG_INT,
              check_ambiguity, override, 0, 0,
              "foo.foo", 'j',
              additional_error))
            goto failure;
        
          break;

        case 0:	/* Long option with no short option */
        case '?':	/* Invalid option.  */
          /* `getopt_long' already printed an error message.  */
          goto failure;

        default:	/* bug: option not considered.  */
          fprintf (stderr, "%s: option unknown: %c%s\n", CANONIZE_NAMES_CMD_PARSER_PACKAGE, c, (additional_error ? additional_error : ""));
          abort ();
        } /* switch */
    } /* while */

  if (args_info->my_group_group_counter > 1)
    {
      fprintf (stderr, "%s: %d options of group my-group were given. At most one is required%s.\n", argv[0], args_info->my_group_group_counter, (additional_error ? additional_error : ""));
      error_occurred = 1;
    }
  
  if (args_info->my_group2_group_counter > 1)
    {
      fprintf (stderr, "%s: %d options of group my.group2 were given. At most one is required%s.\n", argv[0], args_info->my_group2_group_counter, (additional_error ? additional_error : ""));
      error_occurred = 1;
    }
  
  if (args_info->my_group3_group_counter > 1)
    {
      fprintf (stderr, "%s: %d options of group my/group3 were given. At most one is required%s.\n", argv[0], args_info->my_group3_group_counter, (additional_error ? additional_error : ""));
      error_occurred = 1;
    }
  


	FIX_UNUSED(check_required);

  canonize_names_cmd_parser_release (&local_args_info);

  if ( error_occurred )
    return (EXIT_FAILURE);

  return 0;

failure:
  
  canonize_names_cmd_parser_release (&local_args_info);
  return (EXIT_FAILURE);
}
/* vim: set ft=c noet ts=8 sts=8 sw=8 tw=80 nojs spell : */
