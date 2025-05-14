<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRdocsCheckDefaultsDocs`

## 1. Overview:

### Objective and Problem Solved:
The `TFRAMEdocsCheckDefaultsDocs` class is a specialized grid-based user interface component designed to manage and edit a checklist of documents with specific attributes such as required status, description, and status. It provides functionalities for adding, deleting, and validating document entries in a structured grid format. This component is particularly useful in scenarios where users need to manage document checklists with predefined rules and validations.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **cxGrid**: A grid component from DevExpress for displaying and managing tabular data.
- **SOAP Services**: For interacting with external services to fetch or validate data.
- **Custom Components**: Includes custom editors and checkboxes for specific fields.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **docCd**: Document Code (Custom Editor: `cxEDTfind`).
2. **required**: Required Status (Checkbox: `cxCHKrequired`).
3. **descrip**: Description (Read-only).
4. **stat**: Status (Custom Editor: `cxEDTstat`).

#### Grid Actions and Their Effects:
1. **Add**: Adds a new document entry to the grid.
2. **Delete**: Removes the selected document entry from the grid.
3. **Edit**: Allows editing of specific fields in the grid (e.g., `docCd` and `required`).

---

## 2. Functionality Description:

### User/Software Actions:
1. Add a new document entry.
2. Delete an existing document entry.
3. Edit specific fields in the grid.
4. Validate document codes.
5. Search for documents using a custom find dialog.

### Main Components:
- **Grid (`cxDBG`)**: Displays the document checklist.
- **Custom Editors**: Includes a find button (`cxEDTfind`) and a checkbox (`cxCHKrequired`).
- **Action Panel**: Provides buttons for adding and deleting entries.
- **Footer Panel**: Displays additional information or actions.

### Pseudo-code for Actions and Events:
- **OnEditValueChanged**: `if grid cell value changed then validate field`.
- **Add Button Click**: `if add button clicked then add new row to grid`.
- **Delete Button Click**: `if delete button clicked then remove selected row from grid`.
- **Find Button Click**: `if find button clicked then open find dialog`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The grid is configured with hidden fields, field order, read-only fields, and custom editors.
   - Action panel and footer panel are made visible.
   - Event handlers are assigned to specific components.

2. **User Interactions**:
   - Users can add, delete, or edit entries in the grid.
   - Clicking the find button opens a dialog for searching documents.
   - Changes in grid cell values trigger validation.

### Functions and File Locations:
1. **`Create` (Initialization)**:
   - File: `FRdocsCheckDefaultsDocs.pas`
   - Function: `TFRAMEdocsCheckDefaultsDocs.Create`
2. **`SetKeyEditing` (Field Editing Control)**:
   - File: `FRdocsCheckDefaultsDocs.pas`
   - Function: `TFRAMEdocsCheckDefaultsDocs.SetKeyEditing`
3. **`m_FindChkLstDocs` (Find Dialog)**:
   - File: `FRdocsCheckDefaultsDocs.pas`
   - Function: `TFRAMEdocsCheckDefaultsDocs.m_FindChkLstDocs`

### Data Required:
- Document Code (`docCd`).
- Required Status (`required`).
- Description (`descrip`).
- Status (`stat`).

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add**: Enabled at all times.
2. **Delete**: Enabled only when a row is selected.
3. **Edit**: Only specific fields (`docCd`, `required`, `stat`) are editable.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- "Invalid document code" if the document code fails validation.
- "Required field not completed" if mandatory fields are left empty.

### Default Field Values:
- **required**: Default is unchecked (`N`).
- **docCd**: No default value.
- **stat**: No default value.

### Field Validation and Conditions:
- **docCd**: Must be a valid document code.
- **required**: Checkbox with values `Y` (checked) or `N` (unchecked).
- **stat**: Custom editor for status.

---

## 5. Main Functions:

1. **`Create`**: Initializes the grid and its settings.
2. **`SetKeyEditing`**: Controls which fields are editable.
3. **`m_FindChkLstDocs`**: Opens a find dialog for searching documents.
4. **`ValidateCodDoc`**: Validates the document code.

---

## 6. API Service Consumption:

- **Service Name**: `DocCheckListServiceUtils`.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Not explicitly defined in the code.
- **Data Received**: Not explicitly defined in the code.
- **Purpose**: Likely used for validating or fetching document data.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Field**: `required` (Checkbox).
- **Condition**: The checkbox is visible and editable for all rows.

---

## 8. Dependencies:

### External Libraries:
- **DevExpress Components**: For grid and editor functionalities.
- **SOAPHTTPClient**: For interacting with SOAP services.

### Custom Components:
- **`cxEDTfind`**: Custom editor for finding documents.
- **`cxCHKrequired`**: Custom checkbox for required status.

---

## 9. Fields and Validations Listing:

1. **docCd**:
   - Type: String.
   - Required: Yes.
   - Validation: Must be a valid document code.
2. **required**:
   - Type: Checkbox.
   - Required: No.
   - Values: `Y` (checked), `N` (unchecked).
3. **descrip**:
   - Type: String.
   - Required: No.
   - Read-only.
4. **stat**:
   - Type: Custom Editor.
   - Required: No.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```pascal
procedure TFRAMEdocsCheckDefaultsDocs.ACTaddExecute(Sender: TObject);
begin
  // Add a new document entry
end;

procedure TFRAMEdocsCheckDefaultsDocs.ACTdeleteExecute(Sender: TObject);
begin
  // Delete the selected document entry
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Initialization**:
  - `MasterKeyFields := 'cons;consMkt';` defines the key fields for the grid.
  - `AvailableActions := 'ADD;DELETE';` specifies the actions available in the action panel.
- **Grid Settings**:
  - Hidden fields, field order, and read-only fields are configured in the `Create` method.

---

## 12. Conclusion:

The `TFRAMEdocsCheckDefaultsDocs` class provides a robust and customizable grid interface for managing document checklists. Its strengths include flexibility in grid configuration and integration with external services. However, the lack of explicit API details and error handling mechanisms may limit its usability in certain scenarios.

---

## 13. Short Summary:

`TFRAMEdocsCheckDefaultsDocs` is a Delphi-based grid component for managing document checklists with add, delete, and validation functionalities. It integrates custom editors and SOAP services for enhanced usability.#### **FRdocsCheckDefaultsDocs.pas**

```
unit FRdocsCheckDefaultsDocs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel,kneFGFindUtils;

type
  TFRAMEdocsCheckDefaultsDocs = class(TFRAMEBaseGridEditSOA)
    cxCHKrequired: TcxEditRepositoryCheckBoxItem;

    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
    procedure ACTdeleteExecute(Sender: TObject);
  private
    FAddedDocs: String;
    { Private declarations }
    procedure m_FindChkLstDocs(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeChkLstDocs(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure SetAddedDocs(const Value: String);
    procedure ValidateCodDoc;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;

    property AddedDocs: String read FAddedDocs write SetAddedDocs;

    procedure SetKeyEditing(const EditKey: Boolean); override;
  end;

var
  FRAMEdocsCheckDefaultsDocs: TFRAMEdocsCheckDefaultsDocs;

implementation

uses
  kneFindDialogSOA, kneUtils, kneInterfaces, BaseServiceUtils, Global,
  kneTypes, kneFindDialog, kneDialogFactory, kneConfigObjects,
  //---
  DocCheckListServiceUtils;

{$R *.dfm}

constructor TFRAMEdocsCheckDefaultsDocs.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'cons;consMkt';
  DataPacketName := 'CheckListDocDef';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'chkLstDocDef';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';
  PNLfooter.Visible := True;

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Hidden ...........................................................
    HiddenFields.Add('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('docCd;required;descrip;stat');
    // Campos Read-Only ........................................................
    DefineReadOnlyFields('descrip;cons;consName;consMkt;marketDescrip');
    // Key Fields ..............................................................
    KeyFields:= 'cons;consMkt;docCd';
    // Custom Editors ..........................................................
    AddCustomField('docCd','cxEDTfind');
    AddCustomField('required','cxCHKrequired');
    AddCustomField('stat','cxEDTstat');

  end; //with

  ColsWidthInGrid := '80;80;300;100';

  // Atribui��o dos eventos dos Finds
  cxEDTfind.Properties.OnButtonClick := m_FindChkLstDocs;
end;

procedure TFRAMEdocsCheckDefaultsDocs.SetKeyEditing(
  const EditKey: Boolean);
begin
  inherited;
  SetNoEdittingInGridFields('docCd;required;stat', self);
end;


```

#### **FRdocsCheckDefaultsDocs.dfm**

```
inherited FRAMEdocsCheckDefaultsDocs: TFRAMEdocsCheckDefaultsDocs
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
      OptionsView.ShowEditButtons = gsebAlways
    end
  end
  inherited cxEDTR: TcxEditRepository
    inherited cxEDTfind: TcxEditRepositoryButtonItem
      Properties.CharCase = ecUpperCase
    end
    object cxCHKrequired: TcxEditRepositoryCheckBoxItem
      Properties.ValueChecked = 'Y'
      Properties.ValueUnchecked = 'N'
    end
  end
end
```
<!-- tabs:end -->

