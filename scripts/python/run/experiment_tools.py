import os
import datetime
import multiprocessing
from subprocess import call  # call command-line

import run.util as util

__author__ = 'clayton'


# ----------------------------------------------------------------------
# Debug
# ----------------------------------------------------------------------

def test_experiment_tools_access():
    print('TEST test_experiment_tools_access(): experiment_tools accessible.')


# ----------------------------------------------------------------------
# Hamlet root paths
# ----------------------------------------------------------------------

# Relative path from python experiment framework to <hamlet_root>
# This is valid from
#     <hamlet>/hamlet_experiment/scripts/python
# Each domain experiment specified within
#     <hamlet>/hamlet_experiment/scripts/python/experiments
# should have its own local HAMLET_ROOT definition
HAMLET_ROOT = '../../../'

# import os ; print(os.listdir(HAMLET_ROOT))

# ----------------------------------------------------------------------
# The following assume paths relative to <hamlet_root>
# and can be used by any experiment definition that assumes
# the <hamlet> directory structure, assuming a locally
# specified HAMLET_ROOT
DATA_ROOT = 'data/data'
PARAMETERS_ROOT = 'hamlet_experiment/parameters'

# TODO: as of 20160729, hamlet_cpp is hardcoded to put results in '<Hamlet_root>/results/'
# But the following appends to that default path.  Should likely update hamlet_cpp to
# allow specifying the actual results root
RESULTS_ROOT = ''


# ----------------------------------------------------------------------
# Global LOCK
# ----------------------------------------------------------------------

lock = None  # Global definition of lock


# ----------------------------------------------------------------------
# ParameterSpec
# ----------------------------------------------------------------------

# CTM 20170109: model_filename_postfix slot for optionally
# adding a postfix to the model-name directory (for the results)
# that would further differentiate the model based on parameters
# (beyond model class (LT), transition prior type (HDP), dynamics (hmm),
#  and whether learning (w0)
ParameterSpec = util.namedtuple_with_defaults\
    ('ParameterSpec', ['parameters_file', 'parameters_dir',
                       'model_filename_postfix'])


# ----------------------------------------------------------------------
# DataSpec
# ----------------------------------------------------------------------

# TODO provide documentation

DataSpec = util.namedtuple_with_defaults\
    ('DataSpec', ['data_dir', 'data_subdir', 'weights_file'])


# ----------------------------------------------------------------------
# ResultsSpec
# ----------------------------------------------------------------------

ResultsSpec = util.namedtuple_with_defaults\
    ('ResultsSpec', ['results_subdir', 'results_postfix', 'results_dir'])

result_name_abbreviation = \
    {'HMM': 'hmm', 'HSMM': 'hsmm',
     'HDP': 'hdp', 'Dirichlet': 'dir',
     'Known': 'w0', 'IID_normal': 'w1',
     'Normal': 'w1',  # case when weights are being learned, with WEIGHTS_PRIOR = Normal
     'Binary_factorial': 'BFact'}


# ----------------------------------------------------------------------
# ExperimentSpec
# ----------------------------------------------------------------------

class ExperimentSpec:
    """
    Bookkeeping of parameters used in hamlet command-line
    """

    def __init__(self,

                 # possible wholly-formed command, typically specified by process_failures
                 command=None,

                 # specified by process_failures when rerunning experiment
                 rerun_exp_num=None,

                 # specified by collect_experiment_specs
                 # comments indicate hamlet command-line parameter directives
                 parameters_file=None,   # -p
                 parameters_dir=None,    # --parameters_dir

                 data_subdir=None,       # --data_subdir
                 data_dir=None,          # --data_dir     # figures root
                 data_base_name=None,    #

                 results_subdir=None,    # -r
                 results_postfix=None,   # --results_timestamp
                 results_dir=None,       # --results_dir  # results root

                 weights_file=None,      # --weights_file

                 # specified by collect_experiment_specs after above determined
                 exp_num=None,
                 total_exps=None,

                 # specified by run_experiment_wrapper
                 main_path=None,
                 log_file=None,
                 test=None
                 ):

        self.command = command
        self.rerun_exp_num = rerun_exp_num

        self.parameters_file = parameters_file
        self.parameters_dir = parameters_dir

        self.data_dir = data_dir
        self.data_subdir = data_subdir
        self.data_base_name = data_base_name

        self.results_subdir = results_subdir
        self.results_postfix = results_postfix
        self.results_dir = results_dir

        self.weights_file = weights_file

        self.exp_num = exp_num
        self.total_exps = total_exps

        self.main_path = main_path

        self.log_file = log_file
        self.test = test

    def __str__(self):
        s = ''
        if self.command:
            s += 'command={0}, '.format(self.command)
        if self.rerun_exp_num:
            s += 'rerun_exp_num={0}'.format(self.rerun_exp_num)
        if self.parameters_file:
            s += 'parameters_file={0}, '.format(self.parameters_file)
        if self.parameters_dir:
            s += 'parameters_dir={0}, '.format(self.parameters_dir)
        if self.data_dir:
            s += 'data_dir={0}, '.format(self.data_dir)
        if self.data_subdir:
            s += 'data_subdir={0}, '.format(self.data_subdir)
        if self.data_base_name:
            s += 'data_base_name={0}, '.format(self.data_base_name)
        if self.results_dir:
            s += 'results_dir={0}, '.format(self.results_dir)
        if self.results_subdir:
            s += 'results_subdir={0}, '.format(self.results_subdir)
        if self.results_postfix:
            s += 'results_postfix={0}, '.format(self.results_postfix)
        if self.weights_file:
            s += 'weights_file={0}, '.format(self.weights_file)
        if self.exp_num:
            s += 'exp_num={0}, '.format(self.exp_num)
        if self.total_exps:
            s += 'total_exps={0}, '.format(self.total_exps)
        if self.main_path:
            s += 'main_path={0}, '.format(self.main_path)
        if self.log_file:
            s += 'log_file={0}, '.format(self.log_file)
        if self.test:
            s += 'test={0}'.format(self.test)
        return s


# ----------------------------------------------------------------------


# -p parameters_filename
# --parameters_dir parameters_dir

# --weights_file weights_file

# --figures data_file_name
# --data_subdir data_subdir

# -r results_subdir_name
# --results_dir results_dir  # if different from default


def run_experiment(spec):

    global lock

    if spec.command:
        command = spec.command
    else:
        command = './hamlet -p {0}'.format(spec.parameters_file)
        if spec.parameters_dir:
            command += ' --parameters_dir={0}'.format(spec.parameters_dir)
        if spec.data_subdir:
            command += ' --data_subdir={0}'.format(spec.data_subdir)
        if spec.data_dir:
            command += ' --data_dir={0}'.format(spec.data_dir)
        if spec.results_subdir:
            command += ' -r {0}'.format(spec.results_subdir)
        if spec.results_postfix:
            command += ' --results_timestamp={0}'.format(spec.results_postfix)
        if spec.results_dir:
            command += ' --results_dir={0}'.format(spec.results_dir)
        if spec.weights_file:
            command += ' --weights_file={0}'.format(spec.weights_file)

    log_message = "('command', {0}, {1}, '{2}'" \
        .format(spec.exp_num, spec.total_exps, command)

    if spec.rerun_exp_num:
        log_message += ', {0}'.format(spec.rerun_exp_num)

    if spec.test:

        log_message += ')'
        lock.acquire()
        print(log_message)
        with open(spec.log_file, 'a') as logf:
            logf.write(log_message + '\n')
        lock.release()

    owd = os.getcwd()
    os.chdir(spec.main_path)

    ret = -1  # indicates test

    start = datetime.datetime.now()

    # check if target results subdir already exists
    # if so, bail...
    results_path = spec.results_subdir
    if spec.results_dir:
        results_path = spec.results_dir + results_path
    if os.path.isdir(results_path):
        log_message = "('ERROR', 'results_dir_exists', " + log_message + ')'
        if not spec.test:
            log_message += ' )'

        print(log_message)

        os.chdir(owd)

        lock.acquire()
        with open(spec.log_file, 'a') as logf:
            logf.write(log_message + '\n')
        lock.release()

        return spec.exp_num, -2

    elif not spec.test:

        ret = call(command, shell=True)

    end = datetime.datetime.now()

    os.chdir(owd)

    if not spec.test:
        lock.acquire()
        log_message += ", {0}, '{1}')\n".format(ret, end-start)
        with open(spec.log_file, 'a') as logf:
            logf.write(log_message)
        lock.release()

    return spec.exp_num, ret


# ----------------------------------------------------------------------


def get_failures(results_list):
    return [ (exp_num, res) for exp_num, res in results_list if res != 0 ]


def run_experiment_batch(parameter_spec_list,
                         log_file,
                         multiproc=False,
                         processor_pool_size=8,
                         test=True):

    global lock

    results = ''
    start = datetime.datetime.now()

    log_message = "('total_experiments', {0})".format(len(parameter_spec_list))

    if multiproc:

        log_message += "\n('multiprocessing', {0})".format(processor_pool_size)
        print(log_message)
        lock.acquire()
        with open(log_file, 'a') as logf:
            if test: logf.write('RUNNING in TEST MODE\n')
            logf.write(log_message + '\n')
        lock.release()

        p = multiprocessing.Pool(processor_pool_size)
        results = p.map(run_experiment, parameter_spec_list)

    else:

        log_message += "\n('single_process')"
        print(log_message)
        lock.acquire()
        with open(log_file, 'a') as logf:
            if test: logf.write('RUNNING in TEST MODE')
            logf.write(log_message + '\n')
        lock.release()

        for spec in parameter_spec_list:
            run_experiment(spec)

    end = datetime.datetime.now()
    total_time = end - start

    lock.acquire()
    with open(log_file, 'a') as logf:
        if results:
            failures_list = get_failures(results)
            logf.write("('results', {0})\n".format(results))
            if len(failures_list) > 0:
                logf.write("('failures', {0}, {1})\n".format(len(failures_list), failures_list))
        logf.write("('total_time', '{0}')\n".format(total_time))
    lock.release()

    if results: print (results)
    print('\nTotal time: {0}'.format(total_time))


# ----------------------------------------------------------------------


def run_experiment_wrapper(experiment_spec_list,

                           multiproc=False,
                           processor_pool_size=8,

                           rerun_count_max=10,

                           main_path='../',

                           log_file='exp_run',
                           test=True,

                           rerun=True):

    global lock

    lock = multiprocessing.Lock()

    if test: print('RUNNING in TEST MODE')

    log_file += '_' + util.get_timestamp() + '.log'

    print("log file name: '{0}'".format(log_file))

    for spec in experiment_spec_list:
        spec.main_path = main_path
        spec.log_file = log_file
        spec.test = test

    run_experiment_batch(experiment_spec_list,
                         log_file,
                         multiproc=multiproc,
                         processor_pool_size=processor_pool_size,
                         test=test)

    print('DONE.')


# ----------------------------------------------------------------------
# Experiment script helpers
# ----------------------------------------------------------------------


def select_subdirs(dir_branches, match_dict=None, verbose=False):
    """
    selects the cartesian-product of the subdirectories, using match_dict to filter
    which directory paths will be selected
    """

    if verbose:
        print('select_subdirs(): match_dict={0}'.format(match_dict))

    # return dir_branches if no match_dict provided
    if not match_dict:
        return dir_branches

    def get_matched_branch(branch):
        branch_components = branch.strip('/').split('/')
        # matched_branch = list()
        end = 0
        for k in sorted(match_dict.keys()):
            # print k, branch, branch_components, k, match_dict[k], k > len(branch_components),\
            #     branch_components[k] not in match_dict[k]
            if k > len(branch_components):
                return None
            if branch_components[k] not in match_dict[k]:
                return None
            end = k
        return '/'.join(branch_components[0:end+1])

    branches = []
    for branch in dir_branches:
        matched_branch = get_matched_branch(branch)
        if matched_branch is not None and matched_branch not in branches:
            branches.append(matched_branch)

    return branches


def get_dir_branches(root_dir, main_path=HAMLET_ROOT,
                     remove_root_p=True, match_dict=None,
                     select_subdirs_verbose=False):
    """
    Uses os.walk to walk directory tree under root_dir and collects
    subdir branches
    :param root_dir:
    :param remove_root_p:
    :return: list of subdir branches below root_dir
    """

    # select_subdirs_verbose = True

    dir_branches = []

    owd = os.getcwd()
    os.chdir(main_path)

    if not os.path.isdir(root_dir):
        print('WARNING: (experiment_tools.get_dir_branches) Cannot find data root_dir:', root_dir)

    for dirName, subdirList, fileList in os.walk(root_dir):
        if not subdirList:
            if remove_root_p:
                # print 'dirName:', dirName, 'subdirList:', subdirList, 'fileList:', fileList
                without_root = dirName.replace(root_dir, '')  # + '/'
                # print 'without_root', without_root
                dir_branches.append(without_root)
            else:
                dir_branches.append(dirName)  #  + '/')

    os.chdir(owd)

    if select_subdirs_verbose:
        print('dir_branches before selection:')
        for dir_branch in dir_branches:
            print('  {0}'.format(dir_branch))

    # select subset according to match_dict pattern
    # dir_branches = select_subdirs(dir_branches, match_dict=match_dict, verbose=select_subdirs_verbose)
    dir_branches = select_subdirs(dir_branches, match_dict=match_dict, verbose=True)

    if select_subdirs_verbose:
        print('dir_branches after selection:')
        for dir_branch in dir_branches:
            print('  {0}'.format(dir_branch))

    return dir_branches


def collect_data_spec_list(main_path=HAMLET_ROOT,
                           data_dir=None,
                           weights_p=False, match_dict=None,
                           select_subdirs_verbose=False):
    data_spec_list = []

    data_directories = get_dir_branches(data_dir,
                                        main_path=main_path,
                                        remove_root_p=True,
                                        match_dict=match_dict,
                                        select_subdirs_verbose=select_subdirs_verbose)

    for data_subdir in data_directories:
        weights_file = None
        if weights_p:
            weights_file = data_dir + data_subdir + 'weights.txt'
        data_spec_list.append(DataSpec(data_dir, data_subdir, weights_file))

    return data_spec_list


# ----------------------------------------------------------------------

def lt_p(params):
    if 'Isotropic_exponential_similarity:lambda' in params \
            and params['Isotropic_exponential_similarity:lambda'] == '0.0':
        return False
    return True


def results_spec_fn(results_dir, pspec, dspec, replication_postfix,
                    main_path=HAMLET_ROOT):
    """
    Given ParameterSpec and DataSpec, constructs ResultsSpec
    specifying: results_subdir, results_postfix, results_dir
    <results_dir>/<results_subdir>
    <results_subdir> := <results_root_name>/<data_subdir>/<model_subdir>/<postfix>

    :param results_dir:
    :param pspec: ParameterSpec
    :param dspec: DataSpec
    :param replication_postfix: integer representing replication num
    :param main_path:
    :return: ResultsSpec
    """

    results_root_name = results_dir  # dspec.data_dir[0:-1].split('/')[-1]

    if dspec.data_subdir[-1] == '/':
        data_subdir = dspec.data_subdir[0:-1].replace('/', '_')
    else:
        data_subdir = dspec.data_subdir.replace('/', '_')

    pdir = 'parameters/'
    if pspec.parameters_dir:
        # print 'pspec.parameters_dir', pspec.parameters_dir
        pdir = pspec.parameters_dir

    owd = os.getcwd()
    os.chdir(main_path)

    params = util.read_parameter_file_as_dict(pdir, pspec.parameters_file)

    os.chdir(owd)

    dynamics = result_name_abbreviation[params[':MODULE:DYNAMICS']]
    trans_prior = result_name_abbreviation[params[':MODULE:TRANSITION_PRIOR']]
    weights_learned = result_name_abbreviation[params[':MODULE:WEIGHTS_PRIOR']]

    model_subdir = list()

    factorial_p = False
    '''
    if params[':MODULE:TRANSITION_PRIOR'] == 'Binary_factorial':
        factorial_p = True
    '''

    if ':MODEL:MODEL_TYPE' in params and params[':MODEL:MODEL_TYPE'] == 'FACTORIAL':
        factorial_p = True
        trans_prior = 'BFact'

    if factorial_p:
        pass
    elif 'HDP_hyperprior:sticky_c_kappa' in params and 'HDP_hyperprior:sticky_d_kappa' in params:
        if lt_p(params):
            model_subdir.append('StickyLT')
        else:
            model_subdir.append('Sticky')
    elif lt_p(params):
        model_subdir.append('LT')
    else:
        model_subdir.append('noLT')

    model_subdir += [trans_prior, dynamics]
    if weights_learned:
        model_subdir += [ weights_learned ]
    model_subdir = '_'.join(model_subdir)

    if pspec.model_filename_postfix:
        model_subdir += '_{0}'.format(pspec.model_filename_postfix)

    # factorial_p = False
    # # add extra annotation for factorial model
    # if params[':MODULE:TRANSITION_PRIOR'] == 'Binary_factorial':
    #     factorial_p = True
    #     fm = '1'
    #     if 'Binary_factorial:fixed_mean' in params:
    #         fm = params['Binary_factorial:fixed_mean']
    #     if fm == '1':
    #         if 'Binary_factorial:p_mean' in params:
    #             p_mean = params['Binary_factorial:p_mean']
    #         else:
    #             print 'ERROR: was expecting Binary_factorial:p_mean, not found'
    #             sys.exit(-1)
    #         model_subdir += '_fm{0}'.format(p_mean)
    #     else:
    #         model_subdir += '_fmHP'  # indicating using HDP hyperprior params

    # if 'Normal_noise_model:a_h' in params:
    #     if params['Normal_noise_model:a_h'] == '0.1':
    #         model_subdir += '_Nemh01'

    results_subdir = '/'.join([results_root_name, data_subdir, model_subdir])

    results_postfix = '{0:0>2}'.format(replication_postfix)

    return ResultsSpec(results_subdir=results_subdir, results_postfix=results_postfix)


# ----------------------------------------------------------------------

def collect_experiment_spec_list(parameter_spec_list,
                                 data_spec_list,
                                 results_dir,
                                 results_spec_fn=results_spec_fn,
                                 replications=1,
                                 offset=0,
                                 main_path=HAMLET_ROOT):

    experiment_spec_list = []
    for r in range(1, replications + 1):
        for dspec in data_spec_list:
            for pspec in parameter_spec_list:

                rspec = results_spec_fn(results_dir, pspec, dspec, r + offset,
                                        main_path)

                experiment_spec_list.append\
                    (ExperimentSpec
                     (parameters_file=pspec.parameters_file,  # -p
                      parameters_dir=pspec.parameters_dir,    # --parameters_dir

                      data_subdir=dspec.data_subdir,          # --data_subdir
                      data_dir=dspec.data_dir,                # --data_dir
                      weights_file=dspec.weights_file,        # --weights_file

                      results_subdir=rspec.results_subdir,    # -r
                      results_postfix=rspec.results_postfix,  # --results_timestamp
                      results_dir=rspec.results_dir           # --results_dir
                      ) )

    total_exps = len(experiment_spec_list)

    for spec, exp_num in zip(experiment_spec_list, range(1, total_exps + 1)):
        spec.exp_num = exp_num
        spec.total_exps = total_exps

    return experiment_spec_list


# ----------------------------------------------------------------------
# run experiment script
# ----------------------------------------------------------------------

def run_experiment_script(main_path=HAMLET_ROOT,
                          data_dir=None,
                          results_dir=None,
                          replications=1,
                          offset=0,
                          parameter_spec_list=None,
                          # parameter_spec_collector=collect_parameter_spec_list_cp_W0,
                          match_dict=None,
                          multiproc=True,
                          processor_pool_size=multiprocessing.cpu_count(),
                          rerun=True,
                          test=True,
                          select_subdirs_verbose=False):

    # parameter_spec_list = parameter_spec_collector()

    if test:
        print('parameter_spec_list:')
        for i, spec in enumerate(parameter_spec_list):
            print('    [{0}] : {1}'.format(i, spec))

    data_spec_list = collect_data_spec_list(main_path=main_path,
                                            data_dir=data_dir,
                                            match_dict=match_dict,
                                            select_subdirs_verbose=select_subdirs_verbose)

    if test:
        print('data_spec_list:')
        for i, spec in enumerate(data_spec_list):
            print('    [{0}] : {1}'.format(i, spec))

    spec_list = collect_experiment_spec_list \
        (parameter_spec_list=parameter_spec_list,
         data_spec_list=data_spec_list,
         results_dir=results_dir,
         results_spec_fn=results_spec_fn,
         replications=replications,
         offset=offset,
         main_path=main_path)

    if test:

        print('TOTAL EXPERIMENTS: {0}'.format(len(spec_list)))

        print('experiment_spec_list:')
        for i, ps in enumerate(spec_list):
            print('    [{0}] : {1}'.format(i, ps))

        #for spec in spec_list:
        #    print '{0}'.format(spec)

    run_experiment_wrapper \
        (spec_list,
         main_path=main_path,
         multiproc=multiproc,
         processor_pool_size=processor_pool_size,
         log_file='exp_run',
         rerun=rerun,
         test=test)
