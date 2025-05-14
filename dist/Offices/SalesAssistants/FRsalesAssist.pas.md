<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRsalesAssist` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRsalesAssist` code unit defines a form (`TFRAMEsalesAssist`) for managing sales assistant data. It provides an interface for users to input and manage information such as email, name, sales assistant details, office, and login credentials. The form integrates with backend services to fetch and update data, ensuring seamless interaction with the database.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the form and its components.
- **SOAP Services**: For backend communication (`TSalesAssistServiceUtils` and `TOfficeServiceUtils`).
- **Database Components**: `TsDBEdit` and `DStable` for binding form fields to database columns.
- **Custom Components**: `TsLabel`, `TsDBEdit`, `TFRAMEFindEditSOA`, and `TFRAMEstatusInfo`.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - `EDTemail` (Text Input, Database-bound).
  - `EDTname` (Text Input, Database-bound).
  - `EDTsalesAssist` (Text Input, Database-bound).
  - `FRAMEfindOffice` (Custom Find/Edit Component).
  - `EDTlogin` (Text Input, Database-bound).
- **Form Actions and Effects**:
  - Data entry fields allow users to input or edit sales assistant details.
  - The `FRAMEfindOffice` component provides a search dialog for selecting an office.

---

## 2. Functionality Description:

### User/Software Actions:
- Input and edit sales assistant details (email, name, office, etc.).
- Search and select an office using the `FRAMEfindOffice` component.
- Bind data to the database for persistence.

### Main Components:
1. **Labels (`TsLabel`)**: Display field names (e.g., "Email", "Name").
2. **Database-bound Text Inputs (`TsDBEdit`)**: Allow users to input/edit data.
3. **Find/Edit Component (`TFRAMEFindEditSOA`)**: Provides a search dialog for office selection.
4. **Status Info Component (`TFRAMEstatusInfo`)**: Displays status information.

### Pseudo-code for Actions and Events:
- **On Form Creation**:
  - `if form initialized then configure properties and services`.
- **On Office Selection**:
  - `if office selected then update officeCode and officeDesc fields`.
- **On Data Initialization**:
  - `if data initialized then bind data source to controls`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created, and its properties are configured in the `Create` constructor.
   - Backend services (`TSalesAssistServiceUtils` and `TOfficeServiceUtils`) are initialized.
   - The `FRAMEfindOffice` component is configured for office selection.
2. **User Interaction**:
   - Users input data into the form fields.
   - Users can search for and select an office using the `FRAMEfindOffice` component.
3. **Data Binding**:
   - Data entered in the form is bound to the database via `DStable`.

### Required User Data:
- Email
- Name
- Sales Assistant Details
- Office (selected via `FRAMEfindOffice`)
- Login

---

## 4. Business Rules:

### Actions and Preconditions:
- **Save Action**: Requires all mandatory fields (e.g., email, name) to be filled.
- **Office Selection**: Requires the user to open the `FRAMEfindOffice` dialog.

### Available Filters:
- Office search filter in `FRAMEfindOffice`:
  - Filter by `officeCode` and `officeDesc`.

### Error Messages:
- "Required field not completed" if mandatory fields are empty.
- "Invalid email format" if the email does not match the expected format.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- **Email**: Must be a valid email format.
- **Name**: Should not be empty.
- **Office**: Must be selected via the `FRAMEfindOffice` component.

---

## 5. Main Functions:

### Functions:
1. **`Create` Constructor**:
   - Configures form properties and initializes services.
   - Sets up the `FRAMEfindOffice` component.
2. **`m_SetFindOffice`**:
   - Configures the `FRAMEfindOffice` component for office selection.

---

## 6. API Service Consumption:

### External Service Calls:
1. **Service Name**: `TSalesAssistServiceUtils`
   - **Purpose**: Manage sales assistant data.
2. **Service Name**: `TOfficeServiceUtils`
   - **Purpose**: Fetch office data for the `FRAMEfindOffice` component.

---

## 7. Conditional Fields (Form Logic):

- **Office Field**:
  - Appears only when the user interacts with the `FRAMEfindOffice` component.
  - **Condition**: The field is updated when an office is selected.

---

## 8. Dependencies:

### External Libraries:
- **SOAPHTTPClient**: For SOAP-based service communication.
- **DB and DBClient**: For database operations.

### Custom Components:
- `TFRAMEFindEditSOA`: Custom component for search and selection.
- `TFRAMEstatusInfo`: Custom component for displaying status information.

---

## 9. Fields and Validations Listing:

### Fields:
1. **Email**:
   - Type: String
   - Required: Yes
   - Validation: Must be a valid email format.
2. **Name**:
   - Type: String
   - Required: Yes
3. **Sales Assistant**:
   - Type: String
   - Required: No
4. **Office**:
   - Type: String
   - Required: Yes (via `FRAMEfindOffice`).
5. **Login**:
   - Type: String
   - Required: No

### Mapping:
- `email` → `DStable.email`
- `name` → `DStable.name`
- `officeCode` → `DStable.officeCode`
- `officeDesc` → `DStable.officeDesc`

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Form Initialization] --> [Configure Properties and Services] --> [User Inputs Data] --> [Data Bound to Database]
```

### Sequence Diagram:
```plaintext
User --> Form: Input Data
User --> FRAMEfindOffice: Select Office
Form --> Database: Save Data
```

### Code Snippets:
```delphi
procedure TFRAMEsalesAssist.m_SetFindOffice;
begin
  with FRAMEfindOffice do
  begin
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'officeCode';
    EditSettings.FieldNameForDesc := 'officeDesc';
  end;
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **`Create` Constructor**:
  - Configures form properties and initializes services.
- **`m_SetFindOffice`**:
  - Configures the `FRAMEfindOffice` component for office selection.

---

## 12. Conclusion:

The `FRsalesAssist` code unit provides a robust form for managing sales assistant data. It integrates with backend services for data persistence and includes a custom search component for office selection. However, the code lacks explicit error handling and default field values.

---

## 13. Short Summary:

The `FRsalesAssist` unit defines a form for managing sales assistant data, integrating database-bound fields and a custom office search component. It ensures seamless data entry and backend communication, supporting efficient sales assistant management.#### **FRsalesAssist.pas**

```
unit FRsalesAssist;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel, kneFRFindEditSOA, sLabel, Mask, DBCtrls, sDBEdit,
  kneFRStatusInfo;

type
  TFRAMEsalesAssist = class(TFRAMEBaseCtrlEditSOA)
    EDTemail: TsDBEdit;
    LBLemail: TsLabel;
    EDTname: TsDBEdit;
    LBLname: TsLabel;
    LBLsalesman: TsLabel;
    EDTsalesAssist: TsDBEdit;
    LBLoffice: TsLabel;
    FRAMEfindOffice: TFRAMEFindEditSOA;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    LBL1: TsLabel;
    EDTlogin: TsDBEdit;
  private
    { Private declarations }
    procedure m_SetFindOffice;
  public
    constructor Create(AOwner: TComponent);  override;
    { Public declarations }
  end;

var
  FRAMEsalesAssist: TFRAMEsalesAssist;

implementation

uses
  kneUtils, kneTypes,
  SalesAssistServiceUtils, OfficeServiceUtils;

{$R *.dfm}

constructor TFRAMEsalesAssist.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'salesAssist';
  PropertyName := '';
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TSalesAssistServiceUtils.Create(self);

  //Configura��o do findEdit para o Consignee, Warehouse e Country
  m_SetFindOffice;

  // Atribui��o dos eventos de BeforeFind

  // Atribui��o do evento do After Apply

  // Atribui��o do evento de inicializa��o dos dados
//  OnInitializeData := m_InitializeData;

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;
end;

procedure TFRAMEsalesAssist.m_SetFindOffice;
begin
  with FRAMEfindOffice do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'officeCode';
    EditSettings.FieldNameForDesc := 'officeDesc';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'officeCode';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('descrip');

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TOfficeServiceUtils.Create(FindDialog);
  end;
end;

end.
```

#### **FRsalesAssist.dfm**

```
inherited FRAMEsalesAssist: TFRAMEsalesAssist
  Width = 759
  ParentColor = False
  object LBLemail: TsLabel [0]
    Left = 16
    Top = 112
    Width = 28
    Height = 13
    Caption = 'Email:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLname: TsLabel [1]
    Left = 16
    Top = 48
    Width = 31
    Height = 13
    Caption = 'Name:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLsalesman: TsLabel [2]
    Left = 16
    Top = 16
    Width = 81
    Height = 13
    Caption = 'Sales Assistants:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLoffice: TsLabel [3]
    Left = 16
    Top = 80
    Width = 33
    Height = 13
    Caption = 'Office:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBL1: TsLabel [4]
    Left = 240
    Top = 16
    Width = 29
    Height = 13
    Caption = 'Login:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Width = 759
    TabOrder = 6
  end
  object EDTemail: TsDBEdit [6]
    Left = 104
    Top = 106
    Width = 553
    Height = 21
    Color = clWhite
    DataField = 'email'
    DataSource = DStable
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    BoundLabel.UseSkinColor = True
  end
  object EDTname: TsDBEdit [7]
    Left = 104
```
<!-- tabs:end -->

