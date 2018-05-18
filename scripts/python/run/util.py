import collections
import datetime
import os

__author__ = 'clayton'


# ----------------------------------------------------------------------
# named tuple with defaults
# ----------------------------------------------------------------------

# From http://stackoverflow.com/questions/11351032/named-tuple-and-optional-keyword-arguments
def namedtuple_with_defaults(typename, field_names, default_values=None):
    if default_values is None:
        default_values = []
    T = collections.namedtuple(typename, field_names)
    T.__new__.__defaults__ = (None,) * len(T._fields)
    if isinstance(default_values, collections.Mapping):
        prototype = T(**default_values)
    else:
        prototype = T(*default_values)
    T.__new__.__defaults__ = tuple(prototype)
    return T


# ----------------------------------------------------------------------
# get timestamp
# ----------------------------------------------------------------------

def get_timestamp(verbose=False):
    now = datetime.datetime.now()
    # '{0}{month:02d}{2}_{3}{4}{5}'.format(now.year, month=now.month, now.day, now.hour, now.minute, now.second)
    if verbose:
        return 'y={year:04d},m={month:02d},d={day:02d}_h={hour:02d},m={minute:02d},s={second:02d},mu={micro:06d}' \
            .format(year=now.year, month=now.month, day=now.day,
                    hour=now.hour, minute=now.minute, second=now.second,
                    micro=now.microsecond)
    else:
        return '{year:04d}{month:02d}{day:02d}_{hour:02d}{minute:02d}{second:02d}{micro:06d}' \
            .format(year=now.year, month=now.month, day=now.day,
                    hour=now.hour, minute=now.minute, second=now.second,
                    micro=now.microsecond)

# ----------------------------------------------------------------------
# read parameter file as dictionary
# ----------------------------------------------------------------------

def read_parameter_file_as_dict(parameter_filepath, parameter_filename, main_path=None):
    if parameter_filepath[-1] != '/':
        parameter_filepath += '/'

    config_filepath = parameter_filepath + parameter_filename

    '''
    glob_search_path = parameter_filepath + '/*.config'
    config_filepath = glob.glob(glob_search_path)
    if not config_filepath:
        print 'Could not find config file in this glob search path:\n' \
            + '    \'{0}\''.format(glob_search_path)
        sys.exit(1)
    '''

    params_dict = dict()

    if main_path:
        owd = os.getcwd()
        os.chdir(main_path)

    try:
        with open(config_filepath, 'r') as cfile:
            for line in cfile.readlines():
                line = line.rstrip('\n').split(' ')
                if line[0] and not (line[0][0] == '/'):
                    # print '>>> {0}'.format(line)
                    key = line[0] + ':' + line[1]
                    val = line[2]
                    params_dict[key] = val
                # else : print '<<< {0}'.format(line)
    except IOError as err:
        print('ERROR: read_parameter_file_as_dict()')
        print('       Current working directory:', os.getcwd())
        raise IOError(err)

    if main_path:
        os.chdir(owd)

    return params_dict
