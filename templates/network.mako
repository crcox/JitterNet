<%
  # Required
  name = NetInfo['NetworkName']
  layers = NetInfo['layers']
  connections = NetInfo['connections']

  # Optional
  if ('intervals' in NetInfo) and NetInfo['intervals']:
    intervals = "-i {n:d}".format(n = NetInfo['intervals'])
  else:
    intervals = ''

  if ('ticksPerInterval' in NetInfo) and NetInfo['ticksPerInterval']:
    ticksPerInterval = "-t {n:d}".format(n = NetInfo['ticksPerInterval'])
  else:
    ticksPerInterval = ''

  if ('NetworkType' in NetInfo) and NetInfo['NetworkType']:
    netType = NetInfo['NetworkType']
  else:
    netType = ''

  # Format arguments for the addNet command
  netargs = ' '.join([name,intervals,ticksPerInterval,netType]).strip()

%>

addNet ${netargs}

% for GROUP in layers:
${add_group(GROUP)}
% endfor

% for CONNECTION in connections:
${add_connection(CONNECTION)}
% endfor

<%def name="add_group(GROUP)" filter="trim">
<%
    line = ['addGroup',GROUP['name'],GROUP['nunits']]
    if GROUP['type'] in ['INPUT','OUTPUT']:
        line.append(GROUP['type'])
    if GROUP['type'] in ['HIDDEN','OUTPUT']:
    # Always remove bias connection so a custom one can be added later.
        line.append('-BIASED')
    if GROUP['type'] == 'OUTPUT':
        line.append(GROUP['errorType'])
        line.append(GROUP['criterion'])

    output = ' '.join(str(x) for x in line)
%>
${output}
</%def>

<%def name="add_connection(CONNECTION)" filter="trim">
<%
    line = ['connectGroups',CONNECTION['pattern'][0],CONNECTION['pattern'][1]]
    line.extend(['-projection',CONNECTION['projection']])
    if 'mean' in CONNECTION['weights'] and CONNECTION['weights']['mean']:
        line.extend(['-mean',CONNECTION['weights']['mean']])
    else:
        line.extend(['-mean',0])

    if 'range' in CONNECTION['weights']:
        line.extend(['-range',CONNECTION['weights']['range']])
    else:
        line.extend(['-range',1])

    if 'type' in CONNECTION and CONNECTION['type']:
        line.extend(['-type',CONNECTION['type']])

    if 'bidirectional' in CONNECTION['weights'] and CONNECTION['weights']['bidirectional']:
        line.append('-bidirectional')

    output = ' '.join(str(x) for x in line)
%>
${output}
</%def>
