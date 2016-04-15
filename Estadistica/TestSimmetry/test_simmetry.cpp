#include <iostream>
#include <vector>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_sort_vector.h>
#include <gsl/gsl_statistics_double.h>
#include <ctime>
#include <cmath>
#include <cstdlib>
#include "ThreadPool.h"

#if defined (MATHEMATICA)
#include "mathlink.h"
#endif

using namespace std;


template<typename R, typename T, typename Arg>
std::vector<R> parallel_function(std::function<R(T, Arg)> f, T min_val, T max_val, T dt, Arg arg)
{
    // Genera semillas.
    const gsl_rng_type * Ty; 
    gsl_rng * r;

    gsl_rng_env_setup();

    Ty = gsl_rng_default;
    r = gsl_rng_alloc(Ty);
    gsl_rng_set(r, time(NULL));
    
    
    vector<int> random_seeds;
    for (int i=0; i<max_val-min_val; ++i)
        random_seeds.push_back(gsl_rng_get(r));
    
    // Manda threads en paralelo.
    ThreadPool pool(std::thread::hardware_concurrency());
    std::vector<std::future<R>> result;
    std::vector<R> result_values;

    for (T val = min_val; val <= max_val; val += dt)
        result.push_back(pool.enqueue(f, random_seeds[val], arg));

    for (unsigned i = 0; i < result.size(); ++i)
        result_values.push_back(result[i].get());
    
    return result_values;
}

double test_simmetry(int seed, int size)
{
    // Crea vector.
    gsl_vector *o1 = gsl_vector_alloc(size);
    gsl_vector *o2 = gsl_vector_alloc(size);
    
    // Genera números.
    const gsl_rng_type * T; 
    gsl_rng * r;

    gsl_rng_env_setup();

    T = gsl_rng_default;
    r = gsl_rng_alloc(T);
    gsl_rng_set(r, seed);

    for (int i = 0; i < size; i++) 
    {
        double k = gsl_ran_gaussian(r, 1);
        gsl_vector_set(o1, i, k);
    }
    
    // Crea vector o2.
    gsl_vector_memcpy(o2, o1);
    gsl_vector_scale(o2, -1.0);
    
    // Ordena elementos.
    gsl_sort_vector(o1);
    gsl_sort_vector(o2);
    
    double out = gsl_stats_correlation(o1->data, 1, o2->data, 1, size);
    
    gsl_rng_free(r);
    gsl_vector_free(o1);
    gsl_vector_free(o2);
    
    return out;
}

#if defined (MATHEMATICA)

void mathematica_test_symmetry(int experiments, int sample_size)
{
    vector<double> out = parallel_function<double, int, int>(test_simmetry, 0, experiments, 1, sample_size);
    double* out_array = &out[0];
    MLPutReal64List( stdlink, out_array, out.size());
}

int main(int argc, char* argv[])
{
    return MLMain(argc, argv);
}
#else
int main(void)
{
    int experiments = 100;
    int sample_size = 100;
    vector<double> result = parallel_function<double, int, int>(test_simmetry, 0, experiments, 1, sample_size);
    
    for (int i=0; i<10; i++)
    {
        cout << result[i] << endl;
    }
    return 0;
}
#endif