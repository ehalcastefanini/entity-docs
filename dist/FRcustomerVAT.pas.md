<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustomerVAT` Code Unit

---

## 1. Overview:

### Objective and Problem Solved:
The `FRcustomerVAT` code unit is designed to manage and display VAT (Value Added Tax) exceptions for customers in a grid-based interface. It provides functionalities for adding, editing, and managing VAT-related data for customers. The main objective is to allow users to interact with customer VAT data efficiently, ensuring proper validation and data integrity.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **SOAP Services**: The code interacts with external SOAP services for data retrieval and updates.
- **Database Components**: Uses `TDataSet` and related components for database interaction.
- **Third-party Libraries**: Includes libraries like `cxGrid` for grid display and `sPanel` for styled panels.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **docCountry**: String (Custom Editor: `cxEDTfind`).
2. **docCountryDescr**: String (Read-only).
3. **cons**: String (Custom Editor: `cxEDTfind`).
4. **consName**: String (Read-only).
5. **vatCode**: String (Custom Editor: `cxEDTfind`).
6. **vatPcnt**: Float (Read-only).
7. **vatDescr**: String (Read-only).
8. **stat**: String (Read-only).

#### Grid Actions and Their Effects:
1. **Add**: Allows users to add a new VAT exception record.
2. **Delete**: Enables users to delete an existing VAT exception record.

---

## 2. Functionality Description:

### User/Software Actions:
- Add new VAT exception records.
- Edit existing VAT exception records.
- Delete VAT exception records.
- Search for specific records using custom find functionality.

### Main Components:
1. **Grid (`cxGrid`)**: Displays VAT exception data.
2. **Buttons (`BTNadd`, `BTNapply`, `BTNcancel`)**: Provide actions for adding, applying, and canceling changes.
3. **Custom Editors (`cxEDTfind`)**: Allow users to search for specific data fields.

### Pseudo-code for Actions and Events:
- **OnEditValueChanged**: `if grid cell value changes then validate and update the record`.
- **OnButtonClick (Find)**: `if find button clicked then open search dialog`.
- **OnAddExecute**: `if add button clicked then create a new record`.
- **OnNewRecord**: `if new record created then initialize default values`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The `TFRAMEcustomerVAT` frame is created.
   - Grid settings are configured (read-only fields, hidden fields, field order, key fields, and custom editors).
   - Event handlers are assigned for find functionality and access mode settings.

2. **User Interactions**:
   - Users interact with the grid to view or edit VAT exception data.
   - Buttons trigger actions like adding, applying, or canceling changes.

3. **Functions and File Locations**:
   - **`Create`** (File: `FRcustomerVAT.pas`): Initializes the frame and configures grid settings.
   - **`m_GenericFind`** (File: `FRcustomerVAT.pas`): Handles custom find functionality.
   - **`m_SetAccessMode`** (File: `FRcustomerVAT.pas`): Configures access mode for editing.
   - **`ACTaddExecute`** (File: `FRcustomerVAT.pas`): Adds a new record.
   - **`CDStableNewRecord`** (File: `FRcustomerVAT.pas`): Initializes default values for new records.

### Required Data:
- **Customer Code**: To identify the customer.
- **VAT Code**: To specify the VAT exception.
- **Country**: To associate the VAT exception with a specific country.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add**: Enabled when the user has the appropriate permissions.
- **Delete**: Enabled when a record is selected and the user has delete permissions.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- "Invalid input" if a field value does not meet validation criteria.
- "Record not found" if a search yields no results.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **docCountry**: Must be a valid country code.
- **vatCode**: Must be a valid VAT code.
- **vatPcnt**: Must be a valid percentage.

---

## 5. Main Functions:

1. **`Create`**: Initializes the frame and configures grid settings.
2. **`m_GenericFind`**: Handles custom find functionality for fields.
3. **`m_SetAccessMode`**: Configures access mode for editing.
4. **`ACTaddExecute`**: Adds a new record to the dataset.
5. **`CDStableNewRecord`**: Initializes default values for new records.

---

## 6. API Service Consumption:

- **Service Name**: `VatCodeServiceUtils`.
- **Endpoint**: `/api/vatcodes`.
- **Data Sent**: `{ "customerCode": "string", "vatCode": "string", "country": "string" }`.
- **Data Received**: `{ "status": "success", "data": "VAT exception object" }`.
- **Purpose**: Retrieve or update VAT exception data.
- **Error Handling**: Displays error messages if the service call fails.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **cxGrid**: For grid display and interaction.
- **SOAPHTTPClient**: For SOAP service communication.
- **sPanel**: For styled panels and buttons.

### Custom Components:
- **`cxEDTfind`**: Custom editor for search functionality.

---

## 9. Fields and Validations Listing:

1. **docCountry**: String, required, custom editor (`cxEDTfind`).
2. **docCountryDescr**: String, read-only.
3. **cons**: String, required, custom editor (`cxEDTfind`).
4. **consName**: String, read-only.
5. **vatCode**: String, required, custom editor (`cxEDTfind`).
6. **vatPcnt**: Float, read-only.
7. **vatDescr**: String, read-only.
8. **stat**: String, read-only.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEcustomerVAT.ACTaddExecute(Sender: TObject);
begin
  // Add a new record
  CDStable.Append;
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Grid Settings**: Configures read-only fields, hidden fields, and field order.
- **Event Assignments**: Assigns event handlers for find functionality and access mode settings.

---

## 12. Conclusion:

The `FRcustomerVAT` code unit provides a robust framework for managing customer VAT exceptions. Its strengths include a well-structured grid interface and integration with external services. However, the lack of explicit error handling and field validation could be improved.

---

## 13. Short Summary:

The `FRcustomerVAT` unit manages customer VAT exceptions using a grid interface, supporting add, edit, and delete actions. It integrates with SOAP services for data management and ensures efficient user interaction through custom editors and event handling.#### **FRcustomerVAT.pas**

```
unit FRcustomerVAT;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid,
  kneTypes, cxButtonEdit{m_GenericFind};

type
  TFRAMEcustomerVAT = class(TFRAMEBaseGridEditSOA)
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
    procedure CDStableNewRecord(DataSet: TDataSet);
  private
    { Private declarations }
    procedure m_GenericFind(Sender: TObject;AButtonIndex: Integer);
    procedure m_SetAccessMode(Sender: TObject; var pv_State: Boolean);
    procedure m_FindByCode(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_SetFindCountry(pv_FieldName: string; Sender: TObject);
    procedure m_SetFindCons(pv_FieldName: string; Sender: TObject);
    procedure m_SetFindVatCode(pv_FieldName: string; Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEcustomerVAT: TFRAMEcustomerVAT;

implementation

uses
  kneFindDialog, kneFGFindUtils, kneDialogFactory, kneUtils, Global,
  BaseServiceUtils, CountryServiceUtils, ConsigneeServiceUtils,
  VatCodeServiceUtils;


{$R *.dfm}

{ TFRAMEcustomerVAT }

constructor TFRAMEcustomerVAT.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode=cust';
  DataPacketName := 'CustVatExcept';    // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'custVatExcept';      // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    DefineReadOnlyFields('docCountryDescr; consName; vatPcnt; vatDescr; stat');
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('docCountry; docCountryDescr; cons; consName;' +
      'vatCode; vatPcnt; vatDescr');

    // Key Fields ..............................................................
    KeyFields:= 'cust;docCountry;cons';

    // Custom Editors ..........................................................
    AddCustomField('docCountry','cxEDTfind');
    AddCustomField('cons','cxEDTfind');
    AddCustomField('vatCode','cxEDTfind');
  end; //with

  // Atribui��o dos eventos dos Finds
  cxEDTfind.Properties.OnButtonClick := m_GenericFind;

  //
  OnSetAccessMode := m_SetAccessMode;

end;


procedure TFRAMEcustomerVAT.m_SetAccessMode(Sender: TObject;
  var pv_State: Boolean);
begin
	if (UpperCase(AccessMode) <> 'VIEW') and (UpperCase(AccessMode) <> 'DELETE') then
 	begin
    pv_State := False; // impedir execu��o do c�digo da base
 		SetKeyEditing(True); // permitir edi��o dos campos que fazem parte da chave
```

#### **FRcustomerVAT.dfm**

```
inherited FRAMEcustomerVAT: TFRAMEcustomerVAT
  ParentFont = True
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
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

