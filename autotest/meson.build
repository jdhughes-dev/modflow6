test_drive = dependency('test-drive', required : false)
if test_drive.found() and not fc_id.contains('intel')
  tests = [
    'ArrayHandlers',
    'BinaryFileReader',
    'Budget',
    'FeatureFlags',
    'GeomUtil',
    'GridFileReader',
    'HashTable',
    'InputOutput',
    'KeyValueList',
    'List',
    'ListIterator',
    'MathUtil',
    'MawNonVertical',
    'MawNur',
    'MemoryContainerIterator',
    'MemoryStore',
    'Message',
    'PseudoInverse',
    'PtrHashTable',
    'Sim',
    'Smoothing',
    'SVD',
    'SwfUtils',
    'TimeSelect',
    'TimeStepSelect',
    'UzfCellGroup',
    'UzfEtUtil'
  ]

  test_srcs = files(
    'tester.f90',
    'tester_utils.f90',
  )
  foreach t : tests
    test_srcs += files('Test@0@.f90'.format(t.underscorify()))
  endforeach

  tester = executable(
    'tester',
    sources: test_srcs,
    link_with: mf6core,
    dependencies: [test_drive] + dependencies + extra_deps,
  )

  test('Test source modules', tester)

  foreach t : tests
    test(t, tester, args: t)
  endforeach

  # the BMI keeps the simulation in global state, so each BMI test runs in
  # its own process, one at a time, in the minimal simulation
  bmi_tests = {
    'Bmi': [
      'bmi_version',
      'component_name',
      'lifecycle',
      'varset_count',
      'varset_members',
      'varset_unknown',
    ],
  }

  bmi_test_srcs = files('bmitester.f90')
  foreach suite, names : bmi_tests
    bmi_test_srcs += files('Test@0@.f90'.format(suite))
  endforeach

  bmitester = executable(
    'bmitester',
    sources: bmi_test_srcs + bmi_sources,
    link_with: mf6core,
    dependencies: [test_drive] + dependencies + extra_deps,
  )

  bmi_testdir = meson.project_source_root() / '.mf6minsim'
  foreach suite, names : bmi_tests
    foreach name : names
      test(
        '@0@:@1@'.format(suite, name),
        bmitester,
        args: [suite, name],
        workdir: bmi_testdir,
        is_parallel: false,
      )
    endforeach
  endforeach
endif