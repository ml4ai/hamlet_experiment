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

def collect_parameter_spec_list_diarization(parameters_path):
    """
    cp **NO** weight learning (w0), 1500 iterations, D=16, and J=600 for hmm
    works with: cocktail_s16_m12
    :return:
    """
    return [ # experiment_tools.ParameterSpec('cocktail16_inference_BFact_HMM_W0.config', parameters_path),
             # experiment_tools.ParameterSpec('cocktail16_inference_LT_HMM_W0-J600.config', parameters_path),
             experiment_tools.ParameterSpec('diarization_inference_noLT_HMM_W0-J200.config', parameters_path)
    ]


# ----------------------------------------------------------------------
# Scripts
# ----------------------------------------------------------------------

match_select_d = {0: ['asu_test_01']}


# The top-level script to run the diarization test
def d_test(test=True):
    """
    Basic experiment using config diarization_inference_{BFact,LT,no_LT}
    2000 iterations, J=200, {a,b}_h=0.1 (prior over precision of noise)
    :return:
    """

    # TODO: expose num of iterations in run_experiment_script

    experiment_tools.run_experiment_script \
        (main_path=HAMLET_ROOT,
         data_dir=os.path.join(DATA_ROOT, 'diarization/'),
         results_dir=os.path.join(RESULTS_ROOT, 'diarization'),
         replications=1,
         offset=0,
         parameter_spec_list=collect_parameter_spec_list_diarization(PARAMETERS_ROOT),
         match_dict=match_select_d,
         multiproc=True,
         processor_pool_size=multiprocessing.cpu_count(),
         rerun=False,
         test=test,
         select_subdirs_verbose=False)


# Run me!
d_test(test=False)
