install JMC

install async profiler

set kernel options

sysctl kernel.perf_event_paranoid=1

export LD_LIBRARY_PATH="/home/yamo/Téléchargements/async-profiler-2.8.3-linux-x64/build"

benchmark-2018/jmh:run -i 3 -wi 3 -f1 -t1 .*Y2015.* -prof async

benchmark-2018/jmh:run -i 3 -wi 3 -f1 -t1 .*Y2015.* -prof async:output=flamegraph

ps -ef | grep java

These options are added automatically
-XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints

yamo      343926  343905 99 14:24 pts/3    00:00:04 /usr/lib/jvm/java-11-openjdk-amd64/bin/java -Djmh.stack.lines=3 -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints -XX:CompileCommandFile=/tmp/jmh6794078445308792641compilecommand -cp /home/yamo/projects/perso/advent-of-code/target/bg-jobs/sbt_4f7ed93a/job-27/target/fc1a3749/3802b591/benchmark-2018_2.13-0.1.0-SNAPSHOT-jmh.jar:/home/yamo/projects/perso/advent-of-code/target/bg-jobs/sbt_4f7ed93a/target/8cbf66e5/3802b591/benchmark-2018_2.13-0.1.0-SNAPSHOT.jar:/home/yamo/projects/perso/advent-of-code/target/bg-jobs/sbt_4f7ed93a/target/f52d45f6/38e89fb3/benchmark-2018_2.13-0.1.0-SNAPSHOT-tests.jar:/home/yamo/projects/perso/advent-of-code/target/bg-jobs/sbt_4f7ed93a/target/9d4d320b/3588b5ba/scala-library-2.13.4.jar:/home/yamo/projects/perso/advent-of-code/target/bg-jobs/sbt_4f7ed93a/target/9f4ae95b/4d1821aa/jmh-core-1.32.jar:/home/yamo/projects/perso/advent-of-code/target/bg-jobs/sbt_4f7ed93a/target/17fa9557/6b93ee6b/jmh-generator-bytecode-1.32.jar:/home/yamo/projects/perso/advent-of-code/target/bg-jobs/sbt_4f7ed93a/target/6a3794e5/92932010/jmh-generator-reflection-1.32.jar:/home/yamo/projects/perso/advent-of-code/target/bg-jobs/sbt_4f7ed93a/target/16fd5667/733d6edf/jopt-simple-4.6.jar:/home/yamo/projects/perso/advent-of-code/target/bg-jobs/sbt_4f7ed93a/target/4ed1f002/75e15656/commons-math3-3.2.jar:/home/yamo/projects/perso/advent-of-code/target/bg-jobs/sbt_4f7ed93a/target/bfdd1cdf/95b18005/jmh-generator-asm-1.32.jar:/home/yamo/projects/perso/advent-of-code/target/bg-jobs/sbt_4f7ed93a/target/dd8097bd/d25ff657/asm-9.0.jar org.openjdk.jmh.runner.ForkedMain 127.0.0.1 43513
yamo      343966  331928  0 14:24 pts/5    00:00:00 grep --color=auto java


[info] [WARN] Install JVM debug symbols to improve profile accuracy

apt install openjdk-11-dbg

[info] running Puzzles 18

[day=18] (t=1184ms) 814
[day=18] (t=820ms) 924

[day=18] (t=232ms) 814
[day=18] (t=55ms) 924

apt install linux-tools-common
apt install linux-tools-generic
apt install linux-tools-5.15.0-52-generic



# Reference

https://foxstephen.net/flamegraphs-on-the-jvm

