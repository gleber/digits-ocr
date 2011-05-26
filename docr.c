#include <stdio.h>

#include <fann.h>

int FANN_API test_callback(struct fann *ann, struct fann_train_data *train,
                           unsigned int max_epochs, unsigned int epochs_between_reports,
                           float desired_error, unsigned int epochs)
{
  printf("Epochs     %8d. MSE: %.5f. Desired-MSE: %.5f\n", epochs, fann_get_MSE(ann), desired_error);
  return 0;
}

int main()
{
  fann_type *calc_out;
  const unsigned int num_input = 784; // 25*25
  const unsigned int num_output = 10;
  const unsigned int num_layers = 3;
  const unsigned int num_neurons_hidden = 150;
  const float desired_error = (const float) 0.02;
  const unsigned int max_epochs = 10000;
  const unsigned int epochs_between_reports = 50;
  struct fann *ann;
  struct fann_train_data *data;


  printf("Creating network.\n");
  ann = fann_create_standard(num_layers, num_input, num_neurons_hidden, num_output);

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

  fann_save(ann, "digits.net");

  printf("Cleaning up.\n");
  fann_destroy_train(data);
  fann_destroy(ann);

  return 0;
}
