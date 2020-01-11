import sys, getopt
import csv
import StringIO
import glob
import os
import os.path
import re
import argparse
#import pandas as pd
#import matplotlib.pyplot as plt
import numpy as np
from scipy import stats as st

# gambit para python 2.4
class _minmax(object):
    def __init__(self, func):
        self.func = func

    def __call__(self, *seq, **kwargs):
        key = kwargs.pop('key', None)
        if kwargs:
            raise TypeError("only 'key' accepted as a "
                            "keyword argument")
        if key is None:
            return self.func(*seq)
        if len(seq) == 1:
            seq, = seq
        return self.func((key(item), i, item)
                         for i, item in enumerate(seq))[-1]


min = _minmax(min)
max = _minmax(max)


def q(cond, on_true, on_false):
    return {True: on_true, False: on_false}[cond is True]


# ---------------------------------------------------------
# natsort.py: Natural string sorting.
# ---------------------------------------------------------
def tryint(s):
    try:
        return int(s)
    except:
        return s


def alphanum_key(s):
    """ Turn a string into a list of string and number chunks.
        "z23a" -> ["z", 23, "a"]
    """
    print [tryint(c) for c in re.split('([0-9]+)', s)][1]
    return int([tryint(c) for c in re.split('([0-9]+)', s)][1])


def natsorted(l):
    """ Sort the given list in the way that humans expect.
    """
    temp = [(alphanum_key(x), x) for x in l]
    temp.sort()


def main(argv):

    csv.field_size_limit(sys.maxsize)

    # Make the graphs a bit prettier, and bigger
    #pd.set_option('display.mpl_style', 'default')
    #pd.set_option('display.line_width', 5000)
    #pd.set_option('display.max_columns', 60)

    parser = argparse.ArgumentParser(description='Process GRASP / ILS - CC and SRCC result files.')
    parser.add_argument('--folder',
                        help='the folder containing the result files')
    parser.add_argument('--filefilter', default='*.csv', required=False,
                        help='the file extension for result files (default: *.csv)')
    parser.add_argument('--cc', action='store_true', default=False,
                        help='Process CC result files as well? (default: false)')
    parser.add_argument('--rcc', action='store_true', default=False,
                        help='Process SRCC result files as well? (default: false)')
    args = parser.parse_args()
    folder = args.folder
    filter = args.filefilter
    cc = args.cc
    rcc = args.rcc

    args = parser.parse_args()

    # print 'Input dir is ', folder
    # print 'File filter is ', filter
    # print 'Process SRCC results = ', rcc

    processCCResult(folder)


    if cc:
        processCCResult(folder)

    if rcc:
        processSRCCResult(folder)


def processCCResult(folder):
    # CC results
    all_files_summary = dict()
    prefix_name = folder[:folder.rfind('/')]
    experiment_name = prefix_name[prefix_name.rfind('/') + 1:] + '-' + folder[folder.rfind('/') + 1:]
    previous_filename = ""
    for root, subFolders, files in os.walk(folder):
        # sort dirs and files
        subFolders.sort()
        files.sort()

        # print "Processing folder " + ''.join(root)
        if (len(files) and ''.join(root) != folder):
            file_list = []
            file_list.extend(glob.glob(root + "/CC*.csv"))
            file_list.extend(glob.glob(root + "/Node*.csv"))
            count = len(file_list) - 1

            # Process CC results
            if os.path.isfile(root + "/cc-result.txt"):
                input_file = open(root + "/cc-result.txt", "r")

                content = input_file.read()
                reader = csv.reader(StringIO.StringIO(content), delimiter='\n')
                for row in reader:
                    if ''.join(row).find("time spent:") >= 0:
                        line = ''.join(row)
                        # the global time is the total time spent by the algorithm (including the parallel version)
                        global_time = float(line[line.find("time spent:") + 11:])
                        break
                input_file.close

                text_file = open(root + "/summary.txt", "w")
                filename = (root[:root.rfind("/")])
                datetime = root[root.rfind("/") + 1:]
                filename = filename[filename.rfind("/") + 1:]
                text_file.write("CC Summary for graph file: %s\n" % filename)
                if not all_files_summary.has_key(filename):
                    all_files_summary[filename] = []

                best_value = 100000000L
                best_pos_value = 0
                best_neg_value = 0
                best_K = 0
                best_iteration = 0
                best_time = 0
                best_param = ''
                value = 0
                pos_value = 0
                neg_value = 0
                K = 0
                iteration = 0
                time = 0
                total_iter = 0
                total_comb = 0

                while count >= 0:
                    # Process all files from all metaheuristic executions, including those in parallel
                    # print "Processing file " + file_list[count] + "\n"
                    content_file = open(file_list[count], 'r')
                    try:
                        content = content_file.read()

                        reader = csv.reader(StringIO.StringIO(content), delimiter=',')
                        for row in reader:
                            linestring = ''.join(row)
                            column = []
                            for col in row:
                                column.append(col)
                            if linestring.startswith('Best value'):
                                filepath = ''.join(file_list[count])
                                text_file.write(filepath[filepath.rfind("/") + 1:] + ' ' + linestring + '\n')
                                # obtains the best result found by a specific execution of a specific node (can be parallel)
                                value = float(column[1])
                                pos_value = float(column[2])
                                neg_value = float(column[3])
                                K = long(column[4])
                                iteration = long(column[5])
                                time = float(column[6])
                                total_iter = long(column[7])
                                # totalizes the number of visited solutions of all nodes running in parallel
                                total_comb += long(column[8])
                                if value < best_value:
                                    # if the results from the execution of this node are better, replace the data
                                    best_value = value
                                    best_pos_value = pos_value
                                    best_neg_value = neg_value
                                    best_K = K
                                    best_iteration = iteration
                                    best_time = time
                                    best_param = filepath[filepath.rfind("/") + 1:]
                                elif value == best_value and time < best_time:  # or if imbalance equals but execution time is smaller
                                    best_K = K
                                    best_iteration = iteration
                                    best_time = time
                                    best_param = filepath[filepath.rfind("/") + 1:]
                                    best_pos_value = pos_value
                                    best_neg_value = neg_value
                        count = count - 1
                    finally:
                        content_file.close()
                text_file.close()
                all_files_summary[filename].append(str(filename) + "; " + str(datetime) + "; " + str(best_value) + "; " + str(pos_value) + "; " + str(
                        neg_value) + "; " + str(best_K) + "; " + str(iteration) + "; " + str(best_time) + "; " + str(
                        global_time) + "; " + best_param + "; " + str(total_iter) + "; " + str(total_comb))

            # end loop
            # process last file
            # end process CC results
    # print "\nExporting consolidated CC Results to csv file...\n"


    # Save CC results of all executions of all instances to csv file: Time to reach best solution and total time spent
    result_file = open(folder + "/summary.csv", "w")
    #print "Instance, I(P), I(P)+, I(P)-, k, Iter, Local time(s), Global time(s), Params, Total Iter, Total Comb"
    result_file.write(
            "Instance; ExecutionID; I(P); I(P)+; I(P)-; k; IterBestSol; TimeToBestSol; Global time(s); Params; Total Iter; NumVisitedSolutions\n")
    for key in sorted(all_files_summary.iterkeys()):
        #print "%s, %s" % (key, all_files_summary[key])
        execution_list = all_files_summary[key]
        for execution in execution_list:
            result_file.write("%s\n" % (execution))
    result_file.close()

    """
    # re-reads the contents of summary.csv back to pandas dataframe
    df = pd.read_csv(folder + "/summary.csv", sep='; ', encoding="utf-8-sig")  # index_col='Instance',

    grouped_results = df.groupby('Instance')
    avg_results = grouped_results.agg([np.mean, np.median, np.max, lambda x: (np.std(x, ddof=1)/np.sqrt(x.count())) * 1.96])  # , np.std
    print avg_results

    # Obtain mean of each group
    grouped = grouped_results['TimeToBestSol']
    means = grouped.mean()
    # print means

    # Calculate 95% confidence interval for each group => margin of error
    # http://www.wikihow.com/Calculate-Confidence-Interval
    ci_time_best_sol = grouped.aggregate(lambda x: (np.std(x, ddof=1)/np.sqrt(x.count())) * 1.96)
    # confidence interval = means +/- ci
    #print ci_time_best_sol
    #print means + " +/- " + ci

    # http://pandas.pydata.org/pandas-docs/version/0.15.1/generated/pandas.DataFrame.to_latex.html
    # print avg_results.to_latex()
    # http://pandas.pydata.org/pandas-docs/version/0.15.1/generated/pandas.DataFrame.to_csv.html?highlight=to_csv#pandas.DataFrame.to_csv
    avg_results.to_csv(str(experiment_name) + '.csv')

    #print best_results.agg([np.sum, np.mean, np.std])



    print df[['I(P)', 'TimeToBestSol']].plot()



    plt.show(block=True)


    print "------ CC Best results:"
    print "Instance, I(P), I(P)+, I(P)-, k, Iter, Local time(s), Global time(s), Params, Total Iter, Total Comb"
    print "------ CC Average results:"
    print "Instance, Avg I(P), Avg K, Avg Time(s), Avg Iter, Avg combinations, Num executions"
    """


def processSRCCResult(folder):
    # print "\nProcessing RCC Results...\n"
    # Process RCC results
    timeInterval = dict()
    timeCount = dict()
    previous_filename = ""
    avg_ip_const = 0
    avg_value = 0
    avg_k = 0
    avg_time = 0
    avg_count = 0
    avg_iter = 0
    avg_comb = 0
    RCC_all_files_summary = dict()
    RCC_best_file_summary = dict()
    RCC_avg_file_summary = dict()

    for root, subFolders, files in os.walk(folder):
        # sort dirs and files
        subFolders.sort()
        files.sort()

        # print "Processing folder " + ''.join(root)
        if (len(files) and ''.join(root) != folder):
            file_list = []
            file_list.extend(glob.glob(root + "/RCC*.csv"))
            count = len(file_list) - 1
            if os.path.isfile(root + "/rcc-result.txt"):
                input_file = open(root + "/rcc-result.txt", "r")

                content = input_file.read()
                reader = csv.reader(StringIO.StringIO(content), delimiter='\n')
                for row in reader:
                    if ''.join(row).find("time spent:") >= 0:
                        line = ''.join(row)
                        global_time = float(line[line.find("time spent:") + 11:])
                        break
                input_file.close

                text_file = open(root + "/rcc-summary.txt", "w")
                filename = (root[:root.rfind("/")])
                datetime = root[root.rfind("/") + 1:]
                filename = filename[filename.rfind("/") + 1:]
                text_file.write("RCC Summary for graph file: %s\n" % filename)
                local_avg_count = 0
                local_avg_ip_const = 0

                best_value = 1000000L
                best_pos_value = 0
                best_neg_value = 0
                best_K = 0
                best_iteration = 0
                best_time = 0
                best_param = ''

                if count < 0:  # no result files for RCC found (imbalance = 0), uses the same result from CC
                    # print("did not understand why Mario inserts CC result files here ???")
                    file_list.extend(glob.glob(root + "/CC*.csv"))
                    count = len(file_list) - 1

                while count >= 0:

                    content_file = open(file_list[count], 'r')
                    # print "Processing file %s\n" % file_list[count]
                    try:
                        content = content_file.read()

                        reader = csv.reader(StringIO.StringIO(content), delimiter=',')
                        for row in reader:
                            linestring = ''.join(row)
                            column = []
                            for col in row:
                                column.append(col)
                            if linestring.startswith('Best value'):
                                filepath = ''.join(file_list[count])
                                text_file.write(filepath[filepath.rfind("/") + 1:] + ' ' + linestring + '\n')
                                value = float(column[1])
                                pos_value = float(column[2])
                                neg_value = float(column[3])
                                K = long(column[4])
                                iteration = long(column[5])
                                time = float(column[6])
                                total_iter = long(column[7])
                                total_comb = long(column[8])
                                if value < best_value:
                                    best_value = value
                                    best_pos_value = pos_value
                                    best_neg_value = neg_value
                                    best_K = K
                                    best_iteration = iteration
                                    best_time = time
                                    best_param = filepath[filepath.rfind("/") + 1:]
                                elif value == best_value and iteration < best_iteration:
                                    best_K = K
                                    best_iteration = iteration
                                    best_time = time
                                    best_param = filepath[filepath.rfind("/") + 1:]
                                    best_pos_value = pos_value
                                    best_neg_value = neg_value
                            elif linestring.startswith('Average initial I(P)'):
                                # computa o valor medio da solucao inicial da fase construtiva para determinado no da execucao paralela
                                ip_const = float(column[1])
                                local_avg_ip_const = local_avg_ip_const + ip_const
                                local_avg_count = local_avg_count + 1
                        count = count - 1
                    finally:
                        content_file.close()
                text_file.close()
                RCC_all_files_summary[filename + "/" + datetime] = str(best_value) + ", " + str(pos_value) + ", " + str(
                        neg_value) + ", " + str(best_K) + ", " + str(best_time) + ", " + str(
                        global_time) + ", " + best_param + ", " + str(total_iter) + ", " + str(total_comb)
                # armazena os valores de todas as execucoes de um mesmo grafo para calculo da media
                if filename == previous_filename:
                    avg_comb = avg_comb + total_comb
                    avg_ip_const = avg_ip_const + local_avg_ip_const
                    avg_value = avg_value + best_value
                    avg_k = avg_k + best_K
                    avg_time = avg_time + global_time
                    avg_iter = avg_iter + total_iter
                    avg_count = avg_count + 1
                else:
                    if avg_count > 0:
                        # print "storing " + previous_filename
                        RCC_avg_file_summary[previous_filename] = str(avg_ip_const / avg_count) + ", " + str(
                                avg_value / avg_count) + ", " + str(avg_k / avg_count) + ", " + str(
                                avg_time / avg_count) + ", " + str(avg_iter / avg_count) + ", " + str(
                                avg_comb / avg_count) + ", " + str(avg_count)
                        # print "average execution times for file " + previous_filename
                        tdir = "./times"
                        if not os.path.exists(tdir):
                            os.makedirs(tdir)
                        times_file = open(tdir + "/RCC-" + previous_filename + "-executionTimes.txt", "w")
                        for key, value in sorted(timeInterval.items()):
                            times_file.write(str(key) + "," + str(value / timeCount[key]) + "\n")
                        times_file.close()
                        timeInterval = dict()
                        timeCount = dict()
                    avg_comb = total_comb
                    avg_ip_const = local_avg_ip_const
                    avg_value = best_value
                    avg_k = best_K
                    avg_time = global_time
                    avg_iter = total_iter
                    avg_count = 1

                # captura o melhor resultado dadas todas as execucoes de um mesmo grafo
                if RCC_best_file_summary.has_key(filename):
                    element = RCC_best_file_summary[filename]
                    value = float(element[0:element.find(',') - 1])
                    if (best_value < value):
                        RCC_best_file_summary[filename] = str(RCC_all_files_summary[filename + "/" + datetime])
                else:
                    RCC_best_file_summary[filename] = str(RCC_all_files_summary[filename + "/" + datetime])

                previous_filename = filename
            # end process RCC results

            # varre os arquivos da pasta em busca dos intervalos de tempo	- DESATIVADO
            for filename in files:
                filename = os.path.join(root, filename)
                if "timeIntervals" in filename:
                    time_list = dict()
                    input_file_t = open(filename, "r")

                    content_t = input_file_t.read()
                    reader_t = csv.reader(StringIO.StringIO(content_t), delimiter=',')

                    last_time = 0.0
                    for row in reader_t:
                        if len(row) > 3:
                            time_list[float(row[0])] = row
                            last_time = float(row[0])
                    input_file_t.close

                    # print "Time intervals for file " + filename
                    # print "Last time is " + str(last_time)
                    time = 0.0
                    time_list_keys = time_list.keys()
                    time_list_keys.sort()
                    # print "Computed times are " + str(time_list_keys)
                    # print "Time slot(s), I(P), Details (Time(s), I(P), I(P)+, I(P)-, k, Iter)"
                    graph_name = filename[:filename.rfind("/")]
                    graph_name = graph_name[:graph_name.rfind("/")]
                    graph_name = graph_name[graph_name.rfind("/") + 1:]
                    # print "graph name is " + graph_name
                    while time <= last_time:
                        index = min(time_list_keys, key=lambda x: q(x < time, time - x, x))
                        timeInterval[time] = float(timeInterval.get(time, 0.0)) + float(time_list[index][1])
                        timeCount[time] = timeCount.get(time, 0.0) + 1
                        # str(time) + ", " + str(time_list[index][1]) + ", " + str(time_list[index])
                        time += 10.0
                    timeInterval[time] = float(timeInterval.get(time, 0.0)) + float(time_list[last_time][1])
                    timeCount[time] = timeCount.get(time, 0.0) + 1
                    # str(last_time) + ", " + str(time_list[last_time][1]) + ", " + str(time_list[last_time])

    # Print RCC results on display
    result_file = open(folder + "/rcc-summary.txt", "w")
    # print "Instance, RI(P), RI(P)+, RI(P)-, k, Local time(s), Global time(s), Params, Total Iter, Total Comb"
    result_file.write(
        "Filename, RI(P), RI(P)+, RI(P)-, k, Local time(s), Global time(s), Params, Total Iter, Total Comb\n")
    for key in sorted(RCC_all_files_summary.iterkeys()):
        print "%s, %s" % (key, RCC_all_files_summary[key])
        result_file.write("%s, %s\n" % (key, RCC_all_files_summary[key]))
    result_file.close()
    # print "------ RCC Best results:"
    # print "Instance, RI(P), RI(P)+, RI(P)-, k, Local time(s), Global time(s), Params, Total Iter, Total Comb"
    for key in sorted(RCC_best_file_summary.iterkeys()):
        print "%s, %s" % (key, RCC_best_file_summary[key])
    # print "------ RCC Average results:"
    # print "Instance, Avg RI(P) const, Avg RI(P), Avg K, Avg Time(s), Avg Iter, Avg combinations, Num executions"
    for key in sorted(RCC_avg_file_summary.iterkeys()):
        print "%s, %s" % (key, RCC_avg_file_summary[key])
    if avg_count > 0:
        # print "storing " + previous_filename
        RCC_avg_file_summary[previous_filename] = str(avg_ip_const / avg_count) + ", " + str(
                avg_value / avg_count) + ", " + str(avg_k / avg_count) + ", " + str(avg_time / avg_count) + ", " + str(
                avg_iter / avg_count) + ", " + str(avg_comb / avg_count) + ", " + str(avg_count)
        # print "average execution times for file " + previous_filename
        tdir = "./times"
        if not os.path.exists(tdir):
            os.makedirs(tdir)
        times_file = open(tdir + "/" + previous_filename + "-executionTimes.txt", "w")
        for key, value in sorted(timeInterval.items()):
            times_file.write(str(key) + "," + str(value / timeCount[key]) + "\n")
        times_file.close()
        timeInterval = dict()
        timeCount = dict()


if __name__ == "__main__":
    main(sys.argv[1:])
