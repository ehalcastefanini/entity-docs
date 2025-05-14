<!-- tabs:start -->

#### **Documentation**

# Documentação do Código: Unidade `FRbackOffice`

## 1. Visão Geral:

### Objetivo Principal:
O objetivo principal deste código é criar uma interface de usuário para gerenciar informações relacionadas ao "Back Office". Ele fornece um formulário que permite visualizar, editar e selecionar dados relacionados ao "Back Office" e ao "Back Office Assistant". Este formulário é utilizado para facilitar a interação com os dados armazenados em um banco de dados, permitindo que os usuários realizem operações de consulta e edição de forma eficiente.

### Tecnologias Utilizadas:
- **Delphi**: Linguagem de programação utilizada para criar a interface e lógica do formulário.
- **Componentes Visuais**: `TsLabel`, `TsDBEdit`, `TFRAMEstatusInfo`, `TFRAMEFindEditSOA` para criar e gerenciar os elementos visuais.
- **Serviços SOAP**: `TBackOfficeServiceUtils` e `TBoAssistServiceUtils` para comunicação com serviços externos.
- **Banco de Dados**: Utilização de `DataSource` e `DataField` para vincular os campos do formulário aos dados do banco.

### Tipo de Formulário:
Este é um **formulário** com os seguintes elementos:
- **Elementos do Formulário**:
  - `EDTdescrip`: Campo de texto para descrição do "Back Office".
  - `EDTbackoffice`: Campo de texto para o código do "Back Office".
  - `FRAMEFindBOAssistant`: Componente para seleção de um "Back Office Assistant".
- **Ações do Formulário**:
  - Configuração de propriedades de serviço e painel de ações.
  - Seleção de um "Back Office Assistant" através de um diálogo de busca.

---

## 2. Descrição da Funcionalidade:

### Ações Específicas:
- Configuração inicial do formulário e seus componentes.
- Vinculação de campos do formulário aos dados do banco de dados.
- Seleção de um "Back Office Assistant" utilizando um diálogo de busca.

### Componentes Principais:
1. **`EDTdescrip`**: Campo de edição vinculado ao campo `boDescrip` do banco de dados.
2. **`EDTbackoffice`**: Campo de edição vinculado ao campo `backoffice` do banco de dados.
3. **`FRAMEFindBOAssistant`**: Componente para busca e seleção de um "Back Office Assistant".
4. **`FRAMEstatusInfo1`**: Exibe informações de status relacionadas ao "Back Office".

### Pseudo-código:
- Evento `OnCreate` do formulário:
  ```pseudo
  ao criar o formulário:
      configurar propriedades do serviço
      configurar painel de ações como invisível
      configurar o componente de busca do "Back Office Assistant"
      vincular o DataSource ao componente de status
  ```
- Método `m_SetFindBoAssist`:
  ```pseudo
  configurar FRAMEFindBOAssistant:
      vincular DataSource e campos de código e descrição
      configurar opções do diálogo de busca
      instanciar serviço de dados para o diálogo de busca
  ```

---

## 3. Lógica Operacional:

### Fluxo de Execução:
1. **Inicialização**:
   - O formulário é criado e suas propriedades são configuradas no construtor `Create`.
   - O painel de ações é desativado e os serviços são configurados.
   - O componente de busca (`FRAMEFindBOAssistant`) é configurado pelo método `m_SetFindBoAssist`.

2. **Interações do Usuário**:
   - O usuário pode preencher os campos `EDTdescrip` e `EDTbackoffice`.
   - O usuário pode abrir o diálogo de busca para selecionar um "Back Office Assistant".

### Dados Necessários:
- **Campos obrigatórios**:
  - `EDTbackoffice`: Código do "Back Office".
  - `EDTdescrip`: Descrição do "Back Office".

---

## 4. Regras de Negócio:

### Ações e Pré-condições:
- **Seleção de "Back Office Assistant"**:
  - Pré-condição: O diálogo de busca deve estar configurado corretamente.
  - Ação: O usuário seleciona um assistente no diálogo de busca.

### Filtros Disponíveis:
- O diálogo de busca permite filtrar assistentes pelo campo `name`.

### Mensagens de Erro:
- Não há mensagens de erro explícitas definidas no código.

### Valores Padrão dos Campos:
- Não há valores padrão definidos explicitamente no código.

### Validações e Condições dos Campos:
- `EDTdescrip` e `EDTbackoffice`:
  - Devem estar vinculados a um `DataSource`.
  - Não há validações adicionais definidas no código.

---

## 5. Funções Principais:

1. **`Create`**:
   - Configura as propriedades do formulário e inicializa os serviços e componentes.
2. **`m_SetFindBoAssist`**:
   - Configura o componente de busca para o "Back Office Assistant".

---

## 6. Consumo de Serviços API:

- **Serviço**: `TBackOfficeServiceUtils`
  - **Finalidade**: Gerenciar dados do "Back Office".
- **Serviço**: `TBoAssistServiceUtils`
  - **Finalidade**: Gerenciar dados do "Back Office Assistant".

---

## 7. Campos Condicionais (Lógica do Formulário):

- Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

### Bibliotecas Externas:
- `kneFRCtrlEditSOA`, `InvokeRegistry`, `SOAPHTTPClient`: Para comunicação com serviços SOAP.
- `sFrameAdapter`, `sLabel`, `sDBEdit`: Para componentes visuais.

### Componentes Personalizados:
- `TFRAMEstatusInfo`: Exibe informações de status.
- `TFRAMEFindEditSOA`: Componente para busca e seleção.

---

## 9. Listagem de Campos e Validações:

- **`EDTdescrip`**:
  - Tipo: String.
  - Obrigatório: Sim.
  - Vinculado ao campo `boDescrip` do banco de dados.
- **`EDTbackoffice`**:
  - Tipo: String.
  - Obrigatório: Sim.
  - Vinculado ao campo `backoffice` do banco de dados.

---

## 10. Exemplos e Diagramas:

### Diagrama de Fluxo:
Não aplicável.

### Diagrama de Sequência:
Não aplicável.

### Código HTML Representando o Formulário:
```html
<div style="width: 724px; font-family: Tahoma; color: #4D4D4D;">
  <label style="display: block; margin-top: 16px;">Back Office:</label>
  <input type="text" style="width: 121px; text-transform: uppercase;" placeholder="Código do Back Office">
  
  <label style="display: block; margin-top: 16px;">Description:</label>
  <input type="text" style="width: 553px; text-transform: uppercase;" placeholder="Descrição do Back Office">
  
  <label style="display: block; margin-top: 16px;">CSA Manager:</label>
  <input type="text" style="width: 553px;" placeholder="Selecionar Assistente">
</div>
```

---

## 11. Comentários Importantes no Código:

- Configuração do painel de ações:
  ```delphi
  ShowActionPanel := False;
  AvailableActions := '';
  ```
- Configuração do componente de busca:
  ```delphi
  FindDialog.Options.DataSelection.FieldNameForCode := 'boAssist';
  ```

---

## 12. Conclusão:

O código implementa um formulário funcional para gerenciar dados do "Back Office". Ele é bem estruturado e utiliza componentes personalizados para facilitar a interação com os dados. No entanto, faltam validações explícitas e mensagens de erro para melhorar a experiência do usuário.

---

## 13. Resumo Curto:

Formulário Delphi para gerenciar dados do "Back Office", com campos vinculados ao banco de dados e um componente de busca para selecionar assistentes. Utiliza serviços SOAP para comunicação e é configurado para facilitar a edição e consulta de dados.#### **FRbackOffice.pas**

```
unit FRbackOffice;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel, kneFRStatusInfo, Mask, DBCtrls, sDBEdit, sLabel,
  kneFRFindEditSOA;

type
  TFRAMEbackOffice = class(TFRAMEBaseCtrlEditSOA)
    LBLname: TsLabel;
    LBLbackOffice: TsLabel;
    EDTdescrip: TsDBEdit;
    EDTbackoffice: TsDBEdit;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    LBLGeneralManager: TsLabel;
    FRAMEFindBOAssistant: TFRAMEFindEditSOA;
  private
    procedure m_SetFindBoAssist;
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
  end;

var
  FRAMEbackOffice: TFRAMEbackOffice;

implementation

uses
  kneUtils, kneTypes,
  BackOfficeServiceUtils, BoAssistServiceUtils;

{$R *.dfm}

constructor TFRAMEbackOffice.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'BackOffice';
  PropertyName := '';
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TBackOfficeServiceUtils.Create(self);

  //Configura��o do findEdit 
  m_SetFindBoAssist;

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;
  ServiceParams.ShowInactives := True;
end;

procedure TFRAMEbackOffice.m_SetFindBoAssist;
begin
  with FRAMEFindBOAssistant do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'respons';
    EditSettings.FieldNameForDesc := 'respName';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'boAssist';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('name');

    FindDialog.Caption := 'Back Office Assistant Selection';

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TBoAssistServiceUtils.Create(FindDialog);
  end;
end;

end.
```

#### **FRbackOffice.dfm**

```
inherited FRAMEbackOffice: TFRAMEbackOffice
  Width = 724
  object LBLname: TsLabel [0]
    Left = 16
    Top = 48
    Width = 57
    Height = 13
    Caption = 'Description:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLbackOffice: TsLabel [1]
    Left = 16
    Top = 16
    Width = 58
    Height = 13
    Caption = 'Back Office:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLGeneralManager: TsLabel [2]
    Left = 16
    Top = 80
    Width = 69
    Height = 13
    BiDiMode = bdLeftToRight
    Caption = 'CSA Manager:'
    FocusControl = FRAMEFindBOAssistant.DBE
    ParentBiDiMode = False
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Width = 724
    TabOrder = 4
    Visible = False
  end
  object EDTdescrip: TsDBEdit [4]
    Left = 86
    Top = 42
    Width = 553
    Height = 21
    CharCase = ecUpperCase
    Color = clWhite
    DataField = 'boDescrip'
    DataSource = DStable
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
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
  object EDTbackoffice: TsDBEdit [5]
    Left = 86
    Top = 10
    Width = 121
    Height = 21
    CharCase = ecUpperCase
    Color = clWhite
    DataField = 'backoffice'
    DataSource = DStable
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclLeft
```
<!-- tabs:end -->

