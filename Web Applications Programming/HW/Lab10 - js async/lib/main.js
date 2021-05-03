$(function(){
	const rowTemplate = $('#data-table tbody tr');
	rowTemplate.find('td.td-hours').hide();
	rowTemplate.find('td.td-hours-edit').hide();

	/**
	 * Show the data once they are available -- a callback for data model.
	 * @param {*} data Array of records if available, null if error occured.
	 * @param {*} error Error message
	 */
	function showData(data, error)
	{
		if (data) {
			$('#data-table tbody').empty();
			data.forEach(rec => {
				// Create a row for this record...
				const row = rowTemplate.clone();
				row.find('td.td-caption').attr('colspan', '1');
				row.find('td.td-caption').text(rec.caption);
				row.find('td.td-hours span').text(rec.hours);
				row.find('td.td-hours').show();
				row.find('td.td-hours-edit input').attr('name', rec.id).val(rec.hours);

				// Handle edit button click...
				row.find('td.td-hours button').click(function(){
					const tr = $(this).closest('tr');
					tr.find('td.td-hours').hide();
					tr.find('td.td-hours-edit').show();
				});

				// Handle save button click...
				row.find('td.td-hours-edit button').click(function(){
					const tr = $(this).closest('tr');

					// Validate input
					const hours = parseInt( tr.find('input').val() );
					if (isNaN(hours) || hours < 0) {
						window.alert("Given input is not a valid number.");
						return;
					}

					// Invoke async update...
					const button = $(this);
					button.removeClass('btn-success');
					const id = tr.find('input').attr('name');
					model.setHours(id, hours, function(error){
						button.addClass('btn-success');
						if (error) { // async update failed
							window.alert(error);
						} else {
							// Hide edit input and update hours value
							tr.find('td.td-hours-edit').hide();
							tr.find('td.td-hours').show();
							tr.find('td.td-hours span').text(hours);
						}
					});
				});

				$('#data-table tbody').append(row);
			});
		}
		else
			window.alert(error || 'Unknown error occured.');
	}
	

	// Initialization and first load of the data...
	const DataModel = module.exports.DataModel;
	model = new DataModel('restapi/');
	model.getData(showData);

	$('#btn-reload').click(function() {
		$('#data-table tbody').empty();
		$('#data-table tbody').append(rowTemplate);
		model.getData(showData);
	});

	$('#btn-invalidate').click(function() {
		model.invalidate();
	});
});
