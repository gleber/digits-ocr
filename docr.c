#include <stdio.h>

#include <fann.h>

int FANN_API test_callback(struct fann *ann, struct fann_train_data *train,
                           unsigned int max_epochs, unsigned int epochs_between_reports,
                           float desired_error, unsigned int epochs)
{
  printf("Epochs     %8d. MSE: %.5f. Desired-MSE: %.5f\n", epochs, fann_get_MSE(ann), desired_error);
  return 0;
}

int main(int argc, char *argv[])
{
  fann_type *calc_out;
  const unsigned int num_input = 784; // 25*25
  const unsigned int num_output = 10;
  unsigned int num_layers = 4;
  const float desired_error = (const float) 0.02;
  const unsigned int max_epochs = 5000;
  const unsigned int epochs_between_reports = 30;
  struct fann *ann;
  struct fann_train_data *data;
  int i;
  int x = 0;

  num_layers = (argc - 1) + 2;

  unsigned int * neurons = malloc(sizeof(unsigned int[num_layers]));
  neurons[0] = num_input;
  neurons[num_layers-1] = num_output;

  for (i = 1; i < argc; i++) {
    x = atoi(argv[i]);
    neurons[i] = x;
  }

  printf("Creating network.\n");
  ann = fann_create_standard_array(num_layers, neurons);

  data = fann_read_train_from_file("train_fann.txt");

  /* fann_set_activation_steepness_hidden(ann, 1); */
  /* fann_set_activation_steepness_output(ann, 1); */

  /* fann_set_activation_function_hidden(ann, FANN_SIGMOID_SYMMETRIC);  */
  fann_set_activation_function_output(ann, FANN_SIGMOID);

  /* fann_set_train_stop_function(ann, FANN_STOPFUNC_BIT); */
  /* fann_set_bit_fail_limit(ann, 0.01f); */

  fann_init_weights(ann, data);

  printf("Training network.\n");
  fann_train_on_data(ann, data, max_epochs, epochs_between_reports, desired_error);

  char layers[200];
  x = 0;
  for (i = 1; i < num_layers - 1; i++) {
    x += sprintf(&layers[x], "%dhu_", neurons[i]);
  }

  char fn[100];
  sprintf(fn, "digits_%dep_%dts_%dl_%s.net", max_epochs, data->num_data, num_layers, layers);

  fann_save(ann, fn);

  printf("Cleaning up.\n");
  fann_destroy_train(data);
  fann_destroy(ann);

  return 0;
}

