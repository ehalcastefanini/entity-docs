<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRaddressContact` Code Unit

---

## 1. Overview:

### Objective and Problem Solved:
The `FRaddressContact` code unit defines a frame (`TFRAMEaddressContact`) that provides a grid-based interface for managing contact information associated with addresses. It allows users to view, edit, and manage contact details in a structured grid format. The main objective is to facilitate the management of contact data, including adding, editing, and deleting contacts, while ensuring proper validation and user interaction.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **SOAP Services**: For interacting with external services to fetch or update contact data.
- **Database Components**: For managing and displaying data from a database.
- **Third-party Libraries**: Includes `cxGrid` for grid display and `TsPanel` for styled panels.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **contactType**: Custom editor (`cxEDTfind`).
2. **contactNameDummy**: String.
3. **contactDesc**: String.
4. **position**: String.

#### Grid Actions and Their Effects:
1. **Add**: Adds a new contact to the grid.
2. **Delete**: Deletes the selected contact from the grid.
3. **Edit**: Allows editing of specific fields in the grid.

---

## 2. Functionality Description:

### User/Software Actions:
- Add a new contact.
- Edit existing contact details.
- Delete a contact.
- Search for contacts using custom find functionality.

### Main Components:
1. **Grid (`cxGrid`)**: Displays contact data in a tabular format.
2. **Buttons (`BTNadd`, `BTNapply`, `BTNcancel`)**: Provide actions for adding, applying, and canceling changes.
3. **Custom Editors**: Used for specific fields like `contactType`.

### Pseudo-code for Actions and Events:
- **OnEditValueChanged**: `if grid cell value changed then update corresponding data`.
- **OnDataChange**: `if data in the dataset changes then refresh the grid`.
- **OnAddButtonClick**: `if add button clicked then open a new row for data entry`.
- **OnFindButtonClick**: `if find button clicked then execute search logic`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized with predefined settings for the grid, including hidden fields, field order, and key fields.
   - Event handlers are assigned to handle user interactions.

2. **User Interactions**:
   - Users can add, edit, or delete contacts using the provided buttons.
   - Changes in the grid trigger events to update the underlying data.

3. **Functions and File Locations**:
   - `Create` (File: `FRaddressContact.pas`): Initializes the frame and sets up grid properties.
   - `EnableContactsEditing` (File: `FRaddressContact.pas`): Configures which fields are editable in the grid.

### Required User Data:
- Contact type.
- Contact name.
- Contact description.
- Position.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add**: Enabled at all times.
- **Delete**: Enabled only when a contact is selected.
- **Edit**: Enabled only for specific fields.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **contactType**: Uses a custom editor (`cxEDTfind`).
- **contactName**: No explicit validation defined.
- **contactDesc**: No explicit validation defined.
- **position**: No explicit validation defined.

---

## 5. Main Functions:

1. **`Create`**:
   - Initializes the frame and configures grid properties.
   - Sets up hidden fields, field order, and key fields.

2. **`EnableContactsEditing`**:
   - Disables editing for specific fields in the grid.

3. **`m_FindContact`**:
   - Handles the logic for finding a contact when the find button is clicked.

4. **`m_FillContactNameColumn`**:
   - Populates the `contactName` column with a given value.

---

## 6. API Service Consumption:

- **Service Name**: ContactTypeServiceUtils.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Not explicitly defined in the code.
- **Data Received**: Not explicitly defined in the code.
- **Purpose**: Likely used for fetching or updating contact data.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **cxGrid**: For grid display and management.
- **TsPanel**: For styled panels and buttons.

### Custom Components:
- **TFRAMEBaseGridEditSOA**: Base class for the frame, providing common functionality for grid-based forms.

---

## 9. Fields and Validations Listing:

1. **contactType**:
   - Type: Custom editor (`cxEDTfind`).
   - Validation: Not explicitly defined.
2. **contactNameDummy**:
   - Type: String.
   - Validation: Not explicitly defined.
3. **contactDesc**:
   - Type: String.
   - Validation: Not explicitly defined.
4. **position**:
   - Type: String.
   - Validation: Not explicitly defined.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```pascal
procedure TFRAMEaddressContact.EnableContactsEditing;
begin
  SetNoEdittingInGridFields('contactType;contactName;contactNameDummy;contactDesc;position', self);
end;
```

### Screenshots:
The DFM file is provided. Below is the HTML representation of the grid:

```html
<table style="width: 100%; border: 1px solid black; border-collapse: collapse;">
  <thead>
    <tr>
      <th style="width: 100px; border: 1px solid black;">Contact Type</th>
      <th style="width: 200px; border: 1px solid black;">Contact Name</th>
      <th style="width: 200px; border: 1px solid black;">Description</th>
      <th style="width: 70px; border: 1px solid black;">Position</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td style="border: 1px solid black;">Type 1</td>
      <td style="border: 1px solid black;">John Doe</td>
      <td style="border: 1px solid black;">Manager</td>
      <td style="border: 1px solid black;">1</td>
    </tr>
    <tr>
      <td style="border: 1px solid black;">Type 2</td>
      <td style="border: 1px solid black;">Jane Smith</td>
      <td style="border: 1px solid black;">Assistant</td>
      <td style="border: 1px solid black;">2</td>
    </tr>
  </tbody>
</table>
```

---

## 11. Important Comments in the Code:

- `MasterKeyFields` defines the primary keys for the grid.
- `AvailableActions` specifies the actions available in the grid (e.g., ADD, DELETE).
- `DefineHiddenFields` hides specific fields in the grid.

---

## 12. Conclusion:

The `FRaddressContact` code unit provides a robust framework for managing contact data in a grid format. While it offers flexibility and customization, the lack of explicit error handling and validation logic may require additional implementation for production use.

---

## 13. Short Summary:

The `FRaddressContact` unit implements a grid-based interface for managing contact data, supporting add, edit, and delete actions. It leverages Delphi's VCL framework and third-party components for a customizable and user-friendly experience.#### **FRaddressContact.pas**

```
unit FRaddressContact;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel;

type
  TFRAMEaddressContact = class(TFRAMEBaseGridEditSOA)
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure DStableDataChange(Sender: TObject; Field: TField);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    procedure m_FindContact(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeContact(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FillContactNameColumn(const pv_ContName: string);
//    function SetFocusinFieldGrid(pv_FieldName: String): Boolean;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure EnableContactsEditing;
  end;

const
  gc_ColsWidthInGrid = '100;200;200;70';

var
  FRAMEaddressContact: TFRAMEaddressContact;

implementation

uses
  kneInterfaces, kneUtils, BaseServiceUtils, kneFGDBUtils, Global,
  ContactTypeServiceUtils, kneTypes, kneFindDialog, kneDialogFactory,
  kneFGFindUtils, kneFREditSOA, kneConfigObjects;

{$R *.dfm}

{ TFRAMEaddressContact }

constructor TFRAMEaddressContact.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'entityCode;entityType;addrNum';
  DataPacketName := 'Contact';
  PropertyName := 'contacts';
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := 'ADD;DELETE';

  ServiceParams.ShowInactives := True; // #3067 - Mostra os registos Inactive

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    DefineHiddenFields('addrNum;entityCode;entityType;contactName');

    // Ordem Campos ............................................................
    DefineOrderFields('contactType;contactNameDummy;contactDesc;position');

    // Key Fields ..............................................................
    KeyFields:= 'entityCode;entityType;addrNum;contactType;contactName';   // 20-09-2012, n�o faz parte da chave OLD contactDesc';
    // Custom Editors ..........................................................
    AddCustomField('contactType','cxEDTfind');
  end; //with

  // Atribui��o dos eventos dos Finds
  cxEDTfind.Properties.OnButtonClick := m_FindContact;
  ColsWidthInGrid := gc_ColsWidthInGrid;
  
  //@@@@@
  CDStable.Tag := 10;
  DStable.Tag := 10;
end;

// ################ EnableContactsEditing  #############################################
procedure TFRAMEaddressContact.EnableContactsEditing;
begin
  SetNoEdittingInGridFields('contactType;contactName;contactNameDummy;contactDesc;position', self); // #14349
end;


// ################ SetFocusinFieldGrid  #############################################
```

#### **FRaddressContact.dfm**

```
inherited FRAMEaddressContact: TFRAMEaddressContact
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
      Left = 91
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331310063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            63003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            310063319C003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630039181000FF00FF00FF00FF00FF00FF00FF00FF006331
            9C00315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00639CFF00315A
            E700315AE7003131CE003131630063313100FF00FF00FF00FF009C316300315A
            E700315AE700315AE700315AE7009C9CFF00FFFFFF00FFFFFF009C9CFF00315A
            E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063639C00315A
            E700315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00A5B5F700315A
            E700315AE700315AE700315AE70031319C0063313100FF00FF00315AE700315A
            E700639CFF006363FF00639CFF00A5B5F700FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00639CFF00315AE7003131CE0063310000FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE700315AE70063313100FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE7003131CE007B392100FF00FF00315AE7003163
            FF00A5B5F700A5B5F700A5B5F700CEEFF700FFFFFF00FFFFFF00CEEFF700A5B5
            F700A5B5F700A5B5F700315AE700315AE7007B392100FF00FF006363CE00315A
            E7006363FF006363FF00639CCE00A5B5F700FFFFFF00FFFFFF00A5B5F7003163
            FF003163CE00315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
            E700639CFF00639CFF00639CFF00B5D6E700FFFFFF00FFFFFF00A5B5F7003163
            FF003163FF003163FF00315AE70063316300FF00FF00FF00FF00FF00FF006363
            9C00315AE700639CFF009C9CFF00CECEFF00FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00
            FF0063639C00315AE700639CFF00A5B5F700B5D6E700A5B5F700639CFF006363
            CE00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00CE6363006363CE00315AE7003163FF006363FF00315AE7006363
            CE009C636300CE633100FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF006331310063313100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF009C639C00A5B5F70031319C003131630031003100FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C316300F7F7F70063639C0000319C003131CE003131630063313100FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100639CCE006363CE0031319C00315AE700315AE70031319C0039181000FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF003131
            9C00315AE70031319C003163CE00315AE700315AE7003163CE00313163006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0063316300315A
            E70031319C0031639C00315AE700315AE700315AE7003163FF0031319C003131
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00315AE7003131
            CE0031319C00639CFF00639CFF00639CFF00639CFF009C9CFF003163CE003131
            630063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00639CCE006363
            CE009C9CCE00639CFF009C9CFF00639CFF00639CCE00A5B5F700A5B5F7003163
            CE003131310063313100FF00FF00FF00FF00FF00FF00FF00FF009C639C00CECE
            CE00A5B5F700CECEFF00A5B5F700A5B5F7006363CE009C9CCE00CECEFF00639C
            FF0031319C0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF009C9C
            9C009C9CCE009C9CCE00B5D6E70063639C00CE313100A5B5F7009C9CCE00CEEF
            F700639CFF003131630039181000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C639C009C639C009C316300FF00FF00FF00FF00CE636300CECECE009C9C
            CE00CECEFF003163CE003131630063313100FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE636300A5B5
            F7009C9CCE00CECEFF0031319C00313131007B392100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            6300639CCE009C9CFF00B5D6E70031319C0094422900FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE63630063639C006363CE009C9CCE00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLcancelAction: TsPanel
        inherited BTNcancel: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331000063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            9C003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100633163003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630063313100FF00FF00FF00FF00FF00FF00CE6331006331
```
<!-- tabs:end -->

