namespace :timeline do
  desc 'Runs timeline test'
  task :run => ['test'] do
    sh './test'
  end

  desc 'Builds timeline test'
  file 'test' => ['test.lhs'] do
    sh 'ghc -threaded test.lhs -o test'
  end
end

namespace :example do
  desc 'Runs example tests'
  task :run => ['Example'] do
    sh './Example'
  end

  file 'Example' => ['example.lhs'] do
    sh 'ghc -package test-framework -package test-framework-hunit -threaded Example.lhs -o Example '
  end
end
