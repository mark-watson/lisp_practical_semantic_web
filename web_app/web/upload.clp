<clp_include name="./header.clp" />

<div id="centerposition">

<clp_ifdef name="upload-status" session>Last file upload was completed OK.</clp_ifdef>

<h3>File upload to repository</h3>

<br />

	<form method="post" action="do-upload" enctype="multipart/form-data"> 
		<table>
			<tr>
				<td>File to upload:</td>
				<td><input type="file" name="file" size="60"></td>
			</tr>
			<tr>
				<td colspan="2">
					<input type="submit" id="file_dialog_hider" value="Cancel">
					<input type="submit" id="file_dialog_cancel_hider" value="OK">
				</td>
			</tr>
		</table>
	</form>

</div>

<clp_include name="footer.clp" />


