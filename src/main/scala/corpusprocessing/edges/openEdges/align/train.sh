onmt-build-vocab --size 50000 --save_vocab src-vocab.txt src-train.txt
onmt-build-vocab --size 50000 --save_vocab tgt-vocab.txt tgt-train.txt

onmt-main train_and_eval --model_type NMTSmall --auto_config --config data.yml
