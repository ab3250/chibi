/** @file test_multiple_cmd.h
 *  @brief The header file for the command line option parser
 *  generated by GNU Gengetopt 
 *  http://www.gnu.org/software/gengetopt.
 *  DO NOT modify this file, since it can be overwritten
 *  @author GNU Gengetopt */

#ifndef TEST_MULTIPLE_CMD_H
#define TEST_MULTIPLE_CMD_H

/* If we use autoconf.  */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h> /* for FILE */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#ifndef TEST_MULTIPLE_CMD_PARSER_PACKAGE
/** @brief the program name (used for printing errors) */
#define TEST_MULTIPLE_CMD_PARSER_PACKAGE "test_multiple"
#endif

#ifndef TEST_MULTIPLE_CMD_PARSER_PACKAGE_NAME
/** @brief the complete program name (used for help and version) */
#define TEST_MULTIPLE_CMD_PARSER_PACKAGE_NAME "test_multiple"
#endif

#ifndef TEST_MULTIPLE_CMD_PARSER_VERSION
/** @brief the program version */
#define TEST_MULTIPLE_CMD_PARSER_VERSION "1.0"
#endif

/** @brief Where the command line options are stored */
struct gengetopt_args_info
{
  const char *help_help; /**< @brief Print help and exit help description.  */
  const char *version_help; /**< @brief Print version and exit help description.  */
  char ** string_arg;	/**< @brief string option (default='foo').  */
  char ** string_orig;	/**< @brief string option original value given at command line.  */
  unsigned int string_min; /**< @brief string option's minimum occurreces */
  unsigned int string_max; /**< @brief string option's maximum occurreces */
  const char *string_help; /**< @brief string option help description.  */
  int* int_arg;	/**< @brief int option.  */
  char ** int_orig;	/**< @brief int option original value given at command line.  */
  unsigned int int_min; /**< @brief int option's minimum occurreces */
  unsigned int int_max; /**< @brief int option's maximum occurreces */
  const char *int_help; /**< @brief int option help description.  */
  short* short_arg;	/**< @brief short option.  */
  char ** short_orig;	/**< @brief short option original value given at command line.  */
  unsigned int short_min; /**< @brief short option's minimum occurreces */
  unsigned int short_max; /**< @brief short option's maximum occurreces */
  const char *short_help; /**< @brief short option help description.  */
  long* long_arg;	/**< @brief long option.  */
  char ** long_orig;	/**< @brief long option original value given at command line.  */
  unsigned int long_min; /**< @brief long option's minimum occurreces */
  unsigned int long_max; /**< @brief long option's maximum occurreces */
  const char *long_help; /**< @brief long option help description.  */
  #if defined(HAVE_LONG_LONG) || defined(HAVE_LONG_LONG_INT)
  long long int* longlong_arg;	/**< @brief long long option.  */
  #else
  long* longlong_arg;	/**< @brief long long option.  */
  #endif
  char ** longlong_orig;	/**< @brief long long option original value given at command line.  */
  unsigned int longlong_min; /**< @brief long long option's minimum occurreces */
  unsigned int longlong_max; /**< @brief long long option's maximum occurreces */
  const char *longlong_help; /**< @brief long long option help description.  */
  char ** limited_arg;	/**< @brief limited multiple option.  */
  char ** limited_orig;	/**< @brief limited multiple option original value given at command line.  */
  unsigned int limited_min; /**< @brief limited multiple option's minimum occurreces */
  unsigned int limited_max; /**< @brief limited multiple option's maximum occurreces */
  const char *limited_help; /**< @brief limited multiple option help description.  */
  char ** limited_interval_arg;	/**< @brief limited multiple option (with interval).  */
  char ** limited_interval_orig;	/**< @brief limited multiple option (with interval) original value given at command line.  */
  unsigned int limited_interval_min; /**< @brief limited multiple option (with interval)'s minimum occurreces */
  unsigned int limited_interval_max; /**< @brief limited multiple option (with interval)'s maximum occurreces */
  const char *limited_interval_help; /**< @brief limited multiple option (with interval) help description.  */
  char ** big_limited_interval_arg;	/**< @brief limited multiple option (with interval).  */
  char ** big_limited_interval_orig;	/**< @brief limited multiple option (with interval) original value given at command line.  */
  unsigned int big_limited_interval_min; /**< @brief limited multiple option (with interval)'s minimum occurreces */
  unsigned int big_limited_interval_max; /**< @brief limited multiple option (with interval)'s maximum occurreces */
  const char *big_limited_interval_help; /**< @brief limited multiple option (with interval) help description.  */
  char ** limited_open_right_arg;	/**< @brief limited multiple option (with interval right open).  */
  char ** limited_open_right_orig;	/**< @brief limited multiple option (with interval right open) original value given at command line.  */
  unsigned int limited_open_right_min; /**< @brief limited multiple option (with interval right open)'s minimum occurreces */
  unsigned int limited_open_right_max; /**< @brief limited multiple option (with interval right open)'s maximum occurreces */
  const char *limited_open_right_help; /**< @brief limited multiple option (with interval right open) help description.  */
  char ** limited_open_left_arg;	/**< @brief limited multiple option (with interval left open).  */
  char ** limited_open_left_orig;	/**< @brief limited multiple option (with interval left open) original value given at command line.  */
  unsigned int limited_open_left_min; /**< @brief limited multiple option (with interval left open)'s minimum occurreces */
  unsigned int limited_open_left_max; /**< @brief limited multiple option (with interval left open)'s maximum occurreces */
  const char *limited_open_left_help; /**< @brief limited multiple option (with interval left open) help description.  */
  float* float_arg;	/**< @brief float option (default='15000').  */
  char ** float_orig;	/**< @brief float option original value given at command line.  */
  unsigned int float_min; /**< @brief float option's minimum occurreces */
  unsigned int float_max; /**< @brief float option's maximum occurreces */
  const char *float_help; /**< @brief float option help description.  */
  char ** no_short_opt_arg;	/**< @brief string option with no short.  */
  char ** no_short_opt_orig;	/**< @brief string option with no short original value given at command line.  */
  unsigned int no_short_opt_min; /**< @brief string option with no short's minimum occurreces */
  unsigned int no_short_opt_max; /**< @brief string option with no short's maximum occurreces */
  const char *no_short_opt_help; /**< @brief string option with no short help description.  */
  unsigned int noarg_min; /**< @brief multiple option with no arg's minimum occurreces */
  unsigned int noarg_max; /**< @brief multiple option with no arg's maximum occurreces */
  const char *noarg_help; /**< @brief multiple option with no arg help description.  */
  unsigned int noarg_noshort_min; /**< @brief multiple option with no arg and no short's minimum occurreces */
  unsigned int noarg_noshort_max; /**< @brief multiple option with no arg and no short's maximum occurreces */
  const char *noarg_noshort_help; /**< @brief multiple option with no arg and no short help description.  */
  char ** optarg_arg;	/**< @brief multi with optional args.  */
  char ** optarg_orig;	/**< @brief multi with optional args original value given at command line.  */
  unsigned int optarg_min; /**< @brief multi with optional args's minimum occurreces */
  unsigned int optarg_max; /**< @brief multi with optional args's maximum occurreces */
  const char *optarg_help; /**< @brief multi with optional args help description.  */
  char ** optarg_noshort_arg;	/**< @brief multi with optional args and no short.  */
  char ** optarg_noshort_orig;	/**< @brief multi with optional args and no short original value given at command line.  */
  unsigned int optarg_noshort_min; /**< @brief multi with optional args and no short's minimum occurreces */
  unsigned int optarg_noshort_max; /**< @brief multi with optional args and no short's maximum occurreces */
  const char *optarg_noshort_help; /**< @brief multi with optional args and no short help description.  */
  char * file_save_arg;	/**< @brief save the passed options into a file.  */
  char * file_save_orig;	/**< @brief save the passed options into a file original value given at command line.  */
  const char *file_save_help; /**< @brief save the passed options into a file help description.  */
  
  unsigned int help_given ;	/**< @brief Whether help was given.  */
  unsigned int version_given ;	/**< @brief Whether version was given.  */
  unsigned int string_given ;	/**< @brief Whether string was given.  */
  unsigned int int_given ;	/**< @brief Whether int was given.  */
  unsigned int short_given ;	/**< @brief Whether short was given.  */
  unsigned int long_given ;	/**< @brief Whether long was given.  */
  unsigned int longlong_given ;	/**< @brief Whether longlong was given.  */
  unsigned int limited_given ;	/**< @brief Whether limited was given.  */
  unsigned int limited_interval_given ;	/**< @brief Whether limited-interval was given.  */
  unsigned int big_limited_interval_given ;	/**< @brief Whether big-limited-interval was given.  */
  unsigned int limited_open_right_given ;	/**< @brief Whether limited-open-right was given.  */
  unsigned int limited_open_left_given ;	/**< @brief Whether limited-open-left was given.  */
  unsigned int float_given ;	/**< @brief Whether float was given.  */
  unsigned int no_short_opt_given ;	/**< @brief Whether no-short-opt was given.  */
  unsigned int noarg_given ;	/**< @brief Whether noarg was given.  */
  unsigned int noarg_noshort_given ;	/**< @brief Whether noarg-noshort was given.  */
  unsigned int optarg_given ;	/**< @brief Whether optarg was given.  */
  unsigned int optarg_noshort_given ;	/**< @brief Whether optarg-noshort was given.  */
  unsigned int file_save_given ;	/**< @brief Whether file-save was given.  */

  char **inputs ; /**< @brief unnamed options (options without names) */
  unsigned inputs_num ; /**< @brief unnamed options number */
} ;

/** @brief The additional parameters to pass to parser functions */
struct test_multiple_cmd_parser_params
{
  int override; /**< @brief whether to override possibly already present options (default 0) */
  int initialize; /**< @brief whether to initialize the option structure gengetopt_args_info (default 1) */
  int check_required; /**< @brief whether to check that all required options were provided (default 1) */
  int check_ambiguity; /**< @brief whether to check for options already specified in the option structure gengetopt_args_info (default 0) */
  int print_errors; /**< @brief whether getopt_long should print an error message for a bad option (default 1) */
} ;

/** @brief the purpose string of the program */
extern const char *gengetopt_args_info_purpose;
/** @brief the usage string of the program */
extern const char *gengetopt_args_info_usage;
/** @brief the description string of the program */
extern const char *gengetopt_args_info_description;
/** @brief all the lines making the help output */
extern const char *gengetopt_args_info_help[];

/**
 * The command line parser
 * @param argc the number of command line options
 * @param argv the command line options
 * @param args_info the structure where option information will be stored
 * @return 0 if everything went fine, NON 0 if an error took place
 */
int test_multiple_cmd_parser (int argc, char **argv,
  struct gengetopt_args_info *args_info);

/**
 * The command line parser (version with additional parameters - deprecated)
 * @param argc the number of command line options
 * @param argv the command line options
 * @param args_info the structure where option information will be stored
 * @param override whether to override possibly already present options
 * @param initialize whether to initialize the option structure my_args_info
 * @param check_required whether to check that all required options were provided
 * @return 0 if everything went fine, NON 0 if an error took place
 * @deprecated use test_multiple_cmd_parser_ext() instead
 */
int test_multiple_cmd_parser2 (int argc, char **argv,
  struct gengetopt_args_info *args_info,
  int override, int initialize, int check_required);

/**
 * The command line parser (version with additional parameters)
 * @param argc the number of command line options
 * @param argv the command line options
 * @param args_info the structure where option information will be stored
 * @param params additional parameters for the parser
 * @return 0 if everything went fine, NON 0 if an error took place
 */
int test_multiple_cmd_parser_ext (int argc, char **argv,
  struct gengetopt_args_info *args_info,
  struct test_multiple_cmd_parser_params *params);

/**
 * Save the contents of the option struct into an already open FILE stream.
 * @param outfile the stream where to dump options
 * @param args_info the option struct to dump
 * @return 0 if everything went fine, NON 0 if an error took place
 */
int test_multiple_cmd_parser_dump(FILE *outfile,
  struct gengetopt_args_info *args_info);

/**
 * Save the contents of the option struct into a (text) file.
 * This file can be read by the config file parser (if generated by gengetopt)
 * @param filename the file where to save
 * @param args_info the option struct to save
 * @return 0 if everything went fine, NON 0 if an error took place
 */
int test_multiple_cmd_parser_file_save(const char *filename,
  struct gengetopt_args_info *args_info);

/**
 * Print the help
 */
void test_multiple_cmd_parser_print_help(void);
/**
 * Print the version
 */
void test_multiple_cmd_parser_print_version(void);

/**
 * Initializes all the fields a test_multiple_cmd_parser_params structure 
 * to their default values
 * @param params the structure to initialize
 */
void test_multiple_cmd_parser_params_init(struct test_multiple_cmd_parser_params *params);

/**
 * Allocates dynamically a test_multiple_cmd_parser_params structure and initializes
 * all its fields to their default values
 * @return the created and initialized test_multiple_cmd_parser_params structure
 */
struct test_multiple_cmd_parser_params *test_multiple_cmd_parser_params_create(void);

/**
 * Initializes the passed gengetopt_args_info structure's fields
 * (also set default values for options that have a default)
 * @param args_info the structure to initialize
 */
void test_multiple_cmd_parser_init (struct gengetopt_args_info *args_info);
/**
 * Deallocates the string fields of the gengetopt_args_info structure
 * (but does not deallocate the structure itself)
 * @param args_info the structure to deallocate
 */
void test_multiple_cmd_parser_free (struct gengetopt_args_info *args_info);

/**
 * Checks that all the required options were specified
 * @param args_info the structure to check
 * @param prog_name the name of the program that will be used to print
 *   possible errors
 * @return
 */
int test_multiple_cmd_parser_required (struct gengetopt_args_info *args_info,
  const char *prog_name);


#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* TEST_MULTIPLE_CMD_H */
