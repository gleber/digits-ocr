#include <stdio.h>

#include <fann.h>

int FANN_API test_callback(struct fann *ann, struct fann_train_data *train,
                           unsigned int max_epochs, unsigned int epochs_between_reports,
                           float desired_error, unsigned int epochs)
{
  printf("Epochs     %8d. MSE: %.5f. Desired-MSE: %.5f\n", epochs, fann_get_MSE(ann), desired_error);
  return 0;
}

void test(char * fn, struct fann_train_data *data) {
  fann_type *calc_out;
  struct fann *ann;
  int i, j;

  ann = fann_create_from_file(fn);

  /* printf("Testing network. %f\n", fann_test_data(ann, data));  */

  /* int i; */
  /* for(i = 0; i < fann_length_train_data(data); i++) */
  /*   { */
  /*     calc_out = fann_run(ann, data->input[i]); */
  /*     printf("XOR test (%f,%f) -> %f, should be %f, difference=%f\n",  */
  /*            data->input[i][0], data->input[i][1], calc_out[0], data->output[i][0], */
  /*            fann_abs(calc_out[0] - data->output[i][0])); */
  /*   } */

  /* printf("Testing network.\n"); */

  int good = 0;
  int bad = 0;

  for(i = 0; i < data->num_data; i++){
    fann_reset_MSE(ann);
    calc_out = fann_test(ann, data->input[i], data->output[i]);
    int sb = -1;
    for (j = 0; j < 10; j++) {
      if (data->output[i][j] == 1) {
        sb = j;
      }
    }

    int res = -1;
    fann_type rv = -1;
    for (j = 0; j < 10; j++) {
      if (calc_out[j] > rv) {
        res = j;
        rv = calc_out[j];
      }
    }
    if (res == sb) {
      good++;
    } else {
      bad++;
    }
    /* printf("Digits test %d -> %d, should be %d, conf %f\n", */
    /*        i, res, sb, rv); */
  }
  printf("Results: %d / %d, %f\n", good, (good+bad), (float)good / (good+bad));

  /* printf("Cleaning up.\n"); */
  fann_destroy(ann);

  return;
}

int main()
{
  struct fann_train_data *data;
  data = fann_read_train_from_file("test.data");
  char* strings[9];
  strings[0] = "digits_2l_10000ts.net";
  strings[1] = "digits_3l_150hu_10000ts_002e.net";
  strings[2] = "digits_3l_450h_2000ts.net";
  strings[3] = "digits_3l_1000hu_001e_2000ts.net";
  strings[4] = "digits_3l_150hu_2000ts.net";
  strings[5] = "digits_3l_450hu_1000ts_49e.net";
  strings[6] = "digits_3l_1000hu_2000ts.net";
  strings[7] = "digits_3l_300hu_1000tr_500e.net";
  strings[8] = "digits_3l_80hu_1000tr.net";
  int i;
  for (i = 0; i < 9; i++) {
    printf("NN: %s ", strings[i]);
    test(strings[i], data);
  }
  fann_destroy_train(data);
  return 0;
}
