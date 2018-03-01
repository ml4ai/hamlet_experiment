from run import experiment_tools
import multiprocessing
import os

__author__ = 'clayton'


# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------

# Relative path from this file to root of python experiment framework
# If *this* file is relocated, this must be updated.
EXPERIMENT_FRAMEWORK_ROOT = '../../../'

# etools.HAMLET_ROOT provides relative path from python experiment
# framework to <hamlet_root>
# The following provides path from this file to <hamlet_root>
HAMLET_ROOT = os.path.join(EXPERIMENT_FRAMEWORK_ROOT,
                           experiment_tools.HAMLET_ROOT)

DATA_ROOT = experiment_tools.DATA_ROOT
PARAMETERS_ROOT = experiment_tools.PARAMETERS_ROOT
RESULTS_ROOT = experiment_tools.RESULTS_ROOT


# ----------------------------------------------------------------------
# TEST: use to verify relative location to python experiment framework
# and <hamlet_root>
# ----------------------------------------------------------------------

def test():
    experiment_tools.test_experiment_tools_access()
    print('EXPERIMENT_FRAMEWORK_ROOT dir contents:',
          os.listdir(EXPERIMENT_FRAMEWORK_ROOT))
    print('<hamlet_root> dir contents:',
          os.listdir(HAMLET_ROOT))


# ----------------------------------------------------------------------
# Parameter spec list
# ----------------------------------------------------------------------

def collect_parameter_spec_list_cocktail16_w0(parameters_path):
    """
    cp **NO** weight learning (w0), 1500 iterations, D=16, and J=600 for hmm
    works with: cocktail_s16_m12
    :return:
    """
    return [ # experiment_tools.ParameterSpec('cocktail16_inference_BFact_HMM_W0.config', parameters_path),
             # experiment_tools.ParameterSpec('cocktail16_inference_LT_HMM_W0-J600.config', parameters_path),
             experiment_tools.ParameterSpec('cocktail16_inference_noLT_HMM_W0-J600.config', parameters_path)
    ]


# ----------------------------------------------------------------------
# Scripts
# ----------------------------------------------------------------------

# The source data may be nested in a tree of subdirectories
# differentiated by properties of the data.
# For example, the synthetic cocktail_party data has been
# generated under a variety of different emission noise
# conditions (e.g., precision=10.0 (h10.0), precision=5.0 (h5.0), etc...);
# under each of those noise conditions, there are several
# data samples (e.g., cp0, cp1, cp2).
# To enable running of batch experiments across a variety of
# combinations of data sources, the following specifies which
# directories in the directory tree will combined in the experiment.
# match_select_regression starts by specifying at the level-0 (first level)
# of the source data directory the list of noise levels that will
# be included in the experiment; this is followed by the second
# level (level-1) specification of the list of cp# samples.
# The batch experiment will include the Cartesian products of these.
# For example, if doing noise levels 10.0, 5.0, 2.0 and for cp0, cp1, cp2,
# then there would be a total of 9 experiments generated (for this part
# of the experiment generation; other configuration, such as number of
# replications will expand the Cartesian product to more...)
match_select_regression = {0: ['h{0}_nocs'.format(h) for h in [10.0]],
                           1: ['cp{0}'.format(i) for i in range(1)]}


# The top-level script to run the cocktail_party regression test
def cp_regression(test=True):
    """
    Basic experiment using config cocktail16_inference_{BFact,LT,no_LT}
    2000 iterations, J=600, {a,b}_h=0.1 (prior over precision of noise)
    :return:
    """
    experiment_tools.run_experiment_script \
        (main_path=HAMLET_ROOT,
         data_dir=os.path.join(DATA_ROOT, 'cocktail_s16_m12/'),
         results_dir=os.path.join(RESULTS_ROOT, 'cocktail_s16_m12'),
         replications=1,
         offset=0,
         parameter_spec_list=collect_parameter_spec_list_cocktail16_w0(PARAMETERS_ROOT),
         match_dict=match_select_regression,
         multiproc=True,
         processor_pool_size=multiprocessing.cpu_count(),
         rerun=False,
         test=test,
         select_subdirs_verbose=False)


# Run me!
cp_regression(test=True)

