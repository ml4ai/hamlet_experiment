import collections
import datetime

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
